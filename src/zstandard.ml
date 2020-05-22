open Core
open! Ctypes
module Raw = Zstd_bindings.C (Zstd_generated)

exception Error of string
exception Content_size_unknown
exception Content_size_error
exception Not_enough_capacity of int
exception Decompressed_size_exceeds_max_int of Int64.t

let raise_on_error (t : Unsigned.Size_t.t) =
  if Unsigned.UInt.to_int (Raw.isError t) <> 0
  then raise (Error (Ctypes.coerce (ptr char) string (Raw.getErrorName t)))
  else t
;;

let raise_if_already_freed freed name =
  if freed then failwithf "%s used after being free" name ()
;;

let get_frame_content_size =
  let content_size_unknown = Unsigned.ULLong.(sub zero (of_int 1)) in
  let content_size_error = Unsigned.ULLong.(sub zero (of_int 2)) in
  fun ptr length ->
    (* This is the magic number ZSTD_FRAMEHEADERSIZE_MAX *)
    let length = Int.max length 18 |> Unsigned.Size_t.of_int in
    let ull = Raw.getFrameContentSize ptr length in
    if Unsigned.ULLong.compare ull content_size_unknown = 0
    then raise Content_size_unknown
    else if Unsigned.ULLong.compare ull content_size_error = 0
    then raise Content_size_error
    else ull
;;

module Compression_context : sig
  type t

  val create : unit -> t
  val free : t -> unit
  val get_exn : t -> Raw.Context.Compression.t Ctypes.ptr
end = struct
  type t =
    { ptr : Raw.Context.Compression.t Ctypes.ptr
    ; mutable freed : bool
    }

  let create () : t = { ptr = Raw.Context.Compression.create (); freed = false }

  let free t =
    raise_if_already_freed t.freed "Compression context";
    let (_ : Unsigned.size_t) = Raw.Context.Compression.free t.ptr |> raise_on_error in
    t.freed <- true;
    ()
  ;;

  let get_exn t =
    raise_if_already_freed t.freed "Compression context";
    t.ptr
  ;;
end

module Decompression_context : sig
  type t

  val create : unit -> t
  val free : t -> unit
  val get_exn : t -> Raw.Context.Decompression.t Ctypes.ptr
end = struct
  type t =
    { ptr : Raw.Context.Decompression.t Ctypes.ptr
    ; mutable freed : bool
    }

  let create () : t = { ptr = Raw.Context.Decompression.create (); freed = false }

  let free t =
    raise_if_already_freed t.freed "Decompression context";
    let (_ : Unsigned.size_t) = Raw.Context.Decompression.free t.ptr |> raise_on_error in
    t.freed <- true;
    ()
  ;;

  let get_exn t =
    raise_if_already_freed t.freed "Decompression context";
    t.ptr
  ;;
end

module Output = struct
  module Allocated = struct
    type 'a t =
      | In_buffer : int t
      | In_iobuf : (read_write, Iobuf.seek) Iobuf.t -> unit t
      | Allocate_string : Bigstring.t -> string t
      | Allocate_bigstring : Bigstring.t -> Bigstring.t t
  end

  type 'a t =
    | In_buffer :
        { buffer : Bigstring.t
        ; pos : int
        ; len : int
        }
        -> int t
    | In_iobuf : { iobuf : (read_write, Iobuf.seek) Iobuf.t } -> unit t
    | Allocate_string : { size_limit : int option } -> string t
    | Allocate_bigstring : { size_limit : int option } -> Bigstring.t t

  let in_buffer ?(pos = 0) ?len buffer =
    let len =
      match len with
      | Some len -> len
      | None -> Bigstring.length buffer - pos
    in
    In_buffer { buffer; pos; len }
  ;;

  let in_iobuf iobuf = In_iobuf { iobuf }
  let allocate_string ~size_limit = Allocate_string { size_limit }
  let allocate_bigstring ~size_limit = Allocate_bigstring { size_limit }

  let has_capacity (type a) (t : a t) size =
    match t with
    | Allocate_string { size_limit } ->
      Option.value_map size_limit ~default:true ~f:(fun size_limit -> size <= size_limit)
    | Allocate_bigstring { size_limit } ->
      Option.value_map size_limit ~default:true ~f:(fun size_limit -> size <= size_limit)
    | In_buffer { len; _ } -> size <= len
    | In_iobuf { iobuf } -> size <= Iobuf.length iobuf
  ;;

  let prepare (type a) (t : a t) size : _ * _ * a Allocated.t =
    if has_capacity t size then () else raise (Not_enough_capacity size);
    let size_t = Unsigned.Size_t.of_int size in
    match t with
    | Allocate_string _ ->
      let buffer = Bigstring.create size in
      ( Ctypes.to_voidp (Ctypes.bigarray_start Array1 buffer)
      , size_t
      , Allocated.Allocate_string buffer )
    | Allocate_bigstring _ ->
      let buffer = Bigstring.create size in
      ( Ctypes.to_voidp (Ctypes.bigarray_start Array1 buffer)
      , size_t
      , Allocated.Allocate_bigstring buffer )
    | In_buffer { buffer; pos; len } ->
      ( Ctypes.to_voidp (Ctypes.bigarray_start Array1 buffer +@ pos)
      , Unsigned.Size_t.of_int len
      , Allocated.In_buffer )
    | In_iobuf { iobuf } ->
      let buffer = Iobuf.Expert.to_bigstring_shared iobuf in
      let len = Iobuf.length iobuf in
      ( Ctypes.to_voidp (Ctypes.bigarray_start Array1 buffer)
      , Unsigned.Size_t.of_int len
      , Allocated.In_iobuf iobuf )
  ;;

  let return (type a) (t : a Allocated.t) ~(size_or_error : Unsigned.Size_t.t) : a =
    let size_t = raise_on_error size_or_error in
    let size = Unsigned.Size_t.to_int size_t in
    match t with
    | Allocated.Allocate_string buffer -> Bigstring.to_string ~len:size buffer
    | Allocated.Allocate_bigstring buffer ->
      Bigstring.unsafe_destroy_and_resize ~len:size buffer
    | Allocated.In_buffer -> size
    | Allocated.In_iobuf iobuf -> Iobuf.resize ~len:size iobuf
  ;;
end

module Input = struct
  type t =
    { buffer : Bigstring.t
    ; pos : int
    ; len : int
    }

  let check_length ~pos ~len ~slen =
    if not (0 <= pos && pos < slen && 0 <= len && pos + len <= slen)
    then raise_s [%message "Invalid string slice" (pos : int) (len : int) (slen : int)]
  ;;

  let from_bigstring ?(pos = 0) ?len str =
    let slen = Bigstring.length str in
    let len =
      match len with
      | Some len -> len
      | None -> slen - pos
    in
    check_length ~pos ~len ~slen;
    { buffer = str; pos; len }
  ;;

  let from_iobuf iobuf =
    let str = Iobuf.Expert.to_bigstring_shared iobuf in
    from_bigstring str
  ;;

  let from_string ?(pos = 0) ?len s =
    let slen = String.length s in
    let len =
      match len with
      | Some len -> len
      | None -> slen - pos
    in
    check_length ~pos ~len ~slen;
    let buffer = Bigstring.of_string ~pos ~len s in
    { buffer; pos = 0; len }
  ;;

  let from_bytes ?(pos = 0) ?len s =
    let slen = Bytes.length s in
    let len =
      match len with
      | Some len -> len
      | None -> slen - pos
    in
    check_length ~pos ~len ~slen;
    let buffer = Bigstring.of_bytes ~pos ~len s in
    { buffer; pos = 0; len }
  ;;

  let length { len; _ } = len

  let ptr { buffer; pos; _ } =
    Ctypes.to_voidp (Ctypes.bigarray_start Array1 buffer +@ pos)
  ;;
end

let decompressed_size input =
  let ptr = Input.ptr input in
  let length = Input.length input in
  Unsigned.ULLong.to_int64 (get_frame_content_size ptr length)
;;

let compress ~f ~input ~output =
  let input_length = Input.length input |> Unsigned.Size_t.of_int in
  let input_ptr = Input.ptr input in
  let size = Raw.compressBound input_length in
  let ptr, size, prepared = Output.prepare output (Unsigned.Size_t.to_int size) in
  let size_or_error = f ptr size input_ptr input_length in
  Output.return prepared ~size_or_error
;;

let decompress_with_frame_length_check ~f ~input ~output =
  let input_length = Input.length input in
  let input_ptr = Input.ptr input in
  let frame_content_size = decompressed_size input in
  if Int64.(Int.(max_value |> to_int64) < frame_content_size)
  then raise (Decompressed_size_exceeds_max_int frame_content_size);
  let frame_content_size = Int64.to_int_exn frame_content_size in
  let output_ptr, output_length, prepared = Output.prepare output frame_content_size in
  let size_or_error =
    f output_ptr output_length input_ptr (input_length |> Unsigned.Size_t.of_int)
  in
  Output.return prepared ~size_or_error
;;

module With_explicit_context = struct
  let compress (t : Compression_context.t) ~compression_level ~input ~output =
    let compression_ctx = Compression_context.get_exn t in
    let f output_ptr output_length input_ptr input_length =
      Raw.Context.Compression.compress
        compression_ctx
        output_ptr
        output_length
        input_ptr
        input_length
        compression_level
    in
    compress ~f ~input ~output
  ;;

  let decompress (t : Decompression_context.t) ~input ~output =
    let decompression_ctx = Decompression_context.get_exn t in
    let f output_ptr output_length input_ptr input_length =
      Raw.Context.Decompression.decompress
        decompression_ctx
        output_ptr
        output_length
        input_ptr
        input_length
    in
    decompress_with_frame_length_check ~input ~output ~f
  ;;
end

module Simple = struct
  let compress ~compression_level ~input ~output =
    let f output_ptr output_length input_ptr input_length =
      Raw.compress output_ptr output_length input_ptr input_length compression_level
    in
    compress ~f ~input ~output
  ;;

  let decompress ~input ~output =
    let f output_ptr output_length input_ptr input_length =
      Raw.decompress output_ptr output_length input_ptr input_length
    in
    decompress_with_frame_length_check ~f ~input ~output
  ;;
end

module Streaming = struct
  open Raw.Streaming

  module Inbuffer = struct
    type t = [ `Inbuffer ] structure

    let create str ~pos ~len : t =
      let inbuffer = Ctypes.make inbuffer in
      Ctypes.setf
        inbuffer
        inbuf_psrc
        (Ctypes.to_voidp (Ctypes.bigarray_start Array1 str));
      Ctypes.setf inbuffer inbuf_pos (Unsigned.Size_t.of_int pos);
      Ctypes.setf inbuffer inbuf_size (Unsigned.Size_t.of_int (pos + len));
      inbuffer
    ;;
  end

  module Outbuffer = struct
    type t = [ `Outbuffer ] structure

    let create str ~pos ~len : t =
      let outbuffer = Ctypes.make outbuffer in
      Ctypes.setf
        outbuffer
        outbuf_pdst
        (Ctypes.to_voidp (Ctypes.bigarray_start Array1 str));
      Ctypes.setf outbuffer outbuf_pos (Unsigned.Size_t.of_int pos);
      Ctypes.setf outbuffer outbuf_size (Unsigned.Size_t.of_int (pos + len));
      outbuffer
    ;;
  end

  module Compression = struct
    open Raw.Streaming.Compression

    type t =
      { cctx : Raw.Context.Compression.t Ctypes_static.ptr
      ; mutable freed : bool
      }

    let create compress_level =
      let t = { cctx = create (); freed = false } in
      let (_ : Unsigned.size_t) = init t.cctx compress_level |> raise_on_error in
      t
    ;;

    let free t =
      raise_if_already_freed t.freed "Compression context";
      let (_undocumented_retvalue : Unsigned.Size_t.t) = free t.cctx in
      t.freed <- true;
      ()
    ;;

    let get_exn t =
      raise_if_already_freed t.freed "Compression context";
      t.cctx
    ;;

    let compress t ~inbuf ~inpos ~inlen ~outbuf ~outpos ~outlen =
      let cctx = get_exn t in
      let inbuffer = Inbuffer.create inbuf ~pos:inpos ~len:inlen in
      let outbuffer = Outbuffer.create outbuf ~pos:outpos ~len:outlen in
      let (_ : Unsigned.size_t) =
        compress cctx (Ctypes.addr outbuffer) (Ctypes.addr inbuffer) |> raise_on_error
      in
      let new_inpos = Ctypes.getf inbuffer inbuf_pos in
      let new_outpos = Ctypes.getf outbuffer outbuf_pos in
      let used_in = Unsigned.Size_t.to_int new_inpos - inpos in
      let used_out = Unsigned.Size_t.to_int new_outpos - outpos in
      used_in, used_out
    ;;

    let flush t ~outbuf ~outpos ~outlen =
      let cctx = get_exn t in
      let outbuffer = Outbuffer.create outbuf ~pos:outpos ~len:outlen in
      let ret = flushStream cctx (Ctypes.addr outbuffer) |> raise_on_error in
      let bytes_internal = Unsigned.Size_t.to_int ret in
      let new_outpos = Ctypes.getf outbuffer outbuf_pos in
      let used_out = Unsigned.Size_t.to_int new_outpos - outpos in
      bytes_internal, used_out
    ;;

    let endstream t ~outbuf ~outpos ~outlen =
      let cctx = get_exn t in
      let outbuffer = Outbuffer.create outbuf ~pos:outpos ~len:outlen in
      let ret = endStream cctx (Ctypes.addr outbuffer) |> raise_on_error in
      let bytes_internal = Unsigned.Size_t.to_int ret in
      let new_outpos = Ctypes.getf outbuffer outbuf_pos in
      let used_out = Unsigned.Size_t.to_int new_outpos - outpos in
      bytes_internal, used_out
    ;;
  end

  module Decompression = struct
    open Raw.Streaming.Decompression

    type t =
      { dctx : Raw.Context.Decompression.t Ctypes_static.ptr
      ; mutable freed : bool
      }

    let create () =
      let t = { dctx = create (); freed = false } in
      let (_ : Unsigned.size_t) = init t.dctx |> raise_on_error in
      t
    ;;

    let free t =
      raise_if_already_freed t.freed "Decompression context";
      let (_undocumented_retvalue : Unsigned.Size_t.t) = free t.dctx in
      ()
    ;;

    let get_exn t =
      raise_if_already_freed t.freed "Decompression context";
      t.dctx
    ;;

    let decompress t ~inbuf ~inpos ~inlen ~outbuf ~outpos ~outlen =
      let dctx = get_exn t in
      let inbuffer = Inbuffer.create inbuf ~pos:inpos ~len:inlen in
      let outbuffer = Outbuffer.create outbuf ~pos:outpos ~len:outlen in
      let (_ : Unsigned.size_t) =
        decompress dctx (Ctypes.addr outbuffer) (Ctypes.addr inbuffer) |> raise_on_error
      in
      let new_inpos = Ctypes.getf inbuffer inbuf_pos in
      let new_outpos = Ctypes.getf outbuffer outbuf_pos in
      let used_in = Unsigned.Size_t.to_int new_inpos - inpos in
      let used_out = Unsigned.Size_t.to_int new_outpos - outpos in
      used_in, used_out
    ;;
  end
end

module Dictionary = struct
  open Raw.Dictionary

  module Training_algorithm = struct
    module Cover = struct
      type t =
        { k : int
        ; d : int
        ; steps : int
        ; nb_threads : int
        ; split_point : float
        }

      let raw (t : t) =
        let r = Ctypes.make Raw.Dictionary.Cover_params.t in
        Ctypes.setf r Raw.Dictionary.Cover_params.k (Unsigned.UInt.of_int t.k);
        Ctypes.setf r Raw.Dictionary.Cover_params.d (Unsigned.UInt.of_int t.d);
        Ctypes.setf r Raw.Dictionary.Cover_params.steps (Unsigned.UInt.of_int t.steps);
        Ctypes.setf
          r
          Raw.Dictionary.Cover_params.nbThreads
          (Unsigned.UInt.of_int t.nb_threads);
        Ctypes.setf r Raw.Dictionary.Cover_params.splitPoint t.split_point;
        r
      ;;

      let default = { k = 1024; d = 8; steps = 0; nb_threads = 0; split_point = 0.0 }
    end

    module Fast_cover = struct
      type t =
        { k : int
        ; d : int
        ; f : int
        ; steps : int
        ; nb_threads : int
        ; split_point : float
        ; accel : int
        }

      let raw (t : t) =
        let r = Ctypes.make Raw.Dictionary.FastCover_params.t in
        Ctypes.setf r Raw.Dictionary.FastCover_params.k (Unsigned.UInt.of_int t.k);
        Ctypes.setf r Raw.Dictionary.FastCover_params.d (Unsigned.UInt.of_int t.d);
        Ctypes.setf r Raw.Dictionary.FastCover_params.f (Unsigned.UInt.of_int t.f);
        Ctypes.setf
          r
          Raw.Dictionary.FastCover_params.steps
          (Unsigned.UInt.of_int t.steps);
        Ctypes.setf
          r
          Raw.Dictionary.FastCover_params.nbThreads
          (Unsigned.UInt.of_int t.nb_threads);
        Ctypes.setf r Raw.Dictionary.FastCover_params.splitPoint t.split_point;
        Ctypes.setf
          r
          Raw.Dictionary.FastCover_params.accel
          (Unsigned.UInt.of_int t.accel);
        r
      ;;
    end

    type t =
      | Default
      | Cover of Cover.t
      | Fast_cover of Fast_cover.t
  end

  open Training_algorithm

  let train ?(dict_size = 102400) ?(training_algorithm = Default) strings return =
    let dict_buffer, dict_length, prepared = Output.prepare return dict_size in
    let total_size =
      Array.fold strings ~init:0 ~f:(fun acc s -> acc + String.length s)
    in
    let samples_buffer = Bigstring.create total_size in
    let sizes = Ctypes.CArray.make Ctypes.size_t (Array.length strings) in
    let current = ref 0 in
    for i = 0 to Array.length strings - 1 do
      Ctypes.CArray.set sizes i (String.length strings.(i) |> Unsigned.Size_t.of_int);
      let s = strings.(i) in
      for j = 0 to String.length s - 1 do
        Bigstring.set samples_buffer !current s.[j];
        incr current
      done
    done;
    let samples_buffer = Ctypes.to_voidp (Ctypes.bigarray_start Array1 samples_buffer) in
    let sizes = Ctypes.CArray.start sizes in
    let nb_strings = Unsigned.UInt.of_int (Array.length strings) in
    let size_or_error =
      match training_algorithm with
      | Default ->
        trainFromBuffer dict_buffer dict_length samples_buffer sizes nb_strings
      | Cover cover ->
        let cover = Cover.raw cover in
        trainFromBuffer_cover
          dict_buffer
          dict_length
          samples_buffer
          sizes
          nb_strings
          cover
      | Fast_cover cover ->
        let cover = Fast_cover.raw cover in
        trainFromBuffer_fastCover
          dict_buffer
          dict_length
          samples_buffer
          sizes
          nb_strings
          cover
    in
    Output.return prepared ~size_or_error
  ;;
end

module Simple_dictionary = struct
  let compress t ~compression_level ~dictionary ~input ~output =
    let compression_ctx = Compression_context.get_exn t in
    let dictionary_length = Input.length dictionary |> Unsigned.Size_t.of_int in
    let dictionary_ptr = Input.ptr dictionary in
    let f output_ptr output_length input_ptr input_length =
      Raw.Simple_dictionary.compress_usingDict
        compression_ctx
        output_ptr
        output_length
        input_ptr
        input_length
        dictionary_ptr
        dictionary_length
        compression_level
    in
    compress ~f ~input ~output
  ;;

  let decompress t ~dictionary ~input ~output =
    let decompression_ctx = Decompression_context.get_exn t in
    let dictionary_length = Input.length dictionary |> Unsigned.Size_t.of_int in
    let dictionary_ptr = Input.ptr dictionary in
    let f output_ptr output_length input_ptr input_length =
      Raw.Simple_dictionary.decompress_usingDict
        decompression_ctx
        output_ptr
        output_length
        input_ptr
        input_length
        dictionary_ptr
        dictionary_length
    in
    decompress_with_frame_length_check ~f ~input ~output
  ;;
end

module Bulk_processing_dictionary = struct
  module Compression = struct
    type t =
      { ctx : Raw.Bulk_processing_dictionary.Compression.t Ctypes.ptr
      ; input_to_prevent_gc : Input.t
      ; mutable freed : bool
      }

    let create ~dictionary ~compression_level : t =
      let dictionary_length = Input.length dictionary |> Unsigned.Size_t.of_int in
      let dictionary_ptr = Input.ptr dictionary in
      let ctx =
        Raw.Bulk_processing_dictionary.Compression.create
          dictionary_ptr
          dictionary_length
          compression_level
      in
      { ctx; input_to_prevent_gc = dictionary; freed = false }
    ;;

    let free t =
      raise_if_already_freed t.freed "Bulk processing dictionary context";
      let (_ : Unsigned.size_t) =
        Raw.Bulk_processing_dictionary.Compression.free t.ctx |> raise_on_error
      in
      t.freed <- true
    ;;

    let get_exn t =
      raise_if_already_freed t.freed "Bulk processing dictionary context";
      t.ctx
    ;;

    let compress t ~context ~input ~output =
      let processing_ctx = get_exn t in
      let compression_ctx = Compression_context.get_exn context in
      let f output_ptr output_length input_ptr input_length =
        Raw.Bulk_processing_dictionary.Compression.compress
          compression_ctx
          output_ptr
          output_length
          input_ptr
          input_length
          processing_ctx
      in
      compress ~f ~input ~output
    ;;
  end

  module Decompression = struct
    type t =
      { ctx : Raw.Bulk_processing_dictionary.Decompression.t Ctypes.ptr
      ; input_to_prevent_gc : Input.t
      ; mutable freed : bool
      }

    let create ~dictionary : t =
      let dictionary_length = Input.length dictionary |> Unsigned.Size_t.of_int in
      let dictionary_ptr = Input.ptr dictionary in
      let ctx =
        Raw.Bulk_processing_dictionary.Decompression.create
          dictionary_ptr
          dictionary_length
      in
      { ctx; input_to_prevent_gc = dictionary; freed = false }
    ;;

    let free t =
      let (_ : Unsigned.size_t) =
        Raw.Bulk_processing_dictionary.Decompression.free t.ctx |> raise_on_error
      in
      t.freed <- true;
      ()
    ;;

    let get_exn t =
      raise_if_already_freed t.freed "Bulk processing dictionary context";
      t.ctx
    ;;

    let decompress t ~context ~input ~output =
      let processing_ctx = get_exn t in
      let decompression_ctx = Decompression_context.get_exn context in
      let f output_ptr output_length input_ptr input_length =
        Raw.Bulk_processing_dictionary.Decompression.decompress
          decompression_ctx
          output_ptr
          output_length
          input_ptr
          input_length
          processing_ctx
      in
      decompress_with_frame_length_check ~f ~input ~output
    ;;
  end
end
