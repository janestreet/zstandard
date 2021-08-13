open Core

(** zstd, short for Zstandard, is a fast lossless compression algorithm, targeting
    real-time compression scenarios at zlib-level and better compression ratios. The zstd
    compression library provides in-memory compression and decompression functions. The
    library supports compression levels from 1 up to [max_compression_level ()] (currently
    22). Levels >= 20, labeled `--ultra`, should be used with caution, as they require
    more memory.

    Compression can be done in:
    - a single step (described as Simple API)
    - a single step, reusing a context (described as Explicit context)
    - unbounded multiple steps (described as Streaming compression)

    The compression ratio achievable on small data can be highly improved using a
    dictionary in:
    - a single step (described as Simple dictionary API)
    - a single step, reusing a dictionary (described as Bulk-processing dictionary API)

    Also see LZ4.
*)

exception Error of string

(** Might be raised when decompressing a message using a single pass decompression
    function.

    Decompressed size is an optional field of compressed messages, but is always present
    when using a single-pass function such as [Simple.compress],
    [With_explicit_content.comprss], [Simple_dictionary.compress] and
    [With_bulk_dictionary.compress]. If the decompressed size is not present in the
    message, it might be necesary to use streaming mode to decompress the message.*)
exception Content_size_unknown

(** Might be raised when decompressing a message using a single pass decompression
    function, indicating an error when inspecting the message -- for instance, invalid
    magic numbers. *)
exception Content_size_error

(** Compression / decompression functions below might raise this exception if the return
    value they are passed does not have enough capacity. The argument is the required
    capacity. *)
exception Not_enough_capacity of int

(** The size of compressed messages is encoded in as a 64 bit integers. This OCaml library
    internally uses [Int.t] to represent size and can't decode messages whose sizes would
    not fit on an int. For those messages, decompression using the streaming mode is
    recommended. *)
exception Decompressed_size_exceeds_max_int of Int64.t

(** Returns the max possible value for [compression_level] *)
val max_compression_level : unit -> int

(** [compression_output_size_bound x] is the maximum possible output size when doing a
    a single-pass compression of an input of size [x].

    (Single pass means something like [Simple.compress]; maximum possible means the worst
    case of the compression algorithm.). *)
val compression_output_size_bound : Int64.t -> Int64.t

module Output : sig
  (** Zstd exposes multiple API flavors which can be used to transform strings into
      strings. The ['a t] type encodes the various ways to return a string from Zstd's
      functions into the OCaml world. *)
  type 'a t

  (** Passing [in_buffer s ~pos ~len] to Zstd functions will cause them to output their
      result in the buffer [s.(pos) ... s.(pos+len)], and return the actual length that
      was used. *)
  val in_buffer : ?pos:int -> ?len:int -> Bigstring.t -> int t

  (** Passing [in_buffer iobuf] to Zstd functions will cause them to output their result
      in the iobuf. *)
  val in_iobuf : (read_write, Iobuf.seek) Iobuf.t -> unit t

  (** Passing [allocate_string] to Zstd functions will cause them to allocate an ocaml
      string to contain their result. *)
  val allocate_string : size_limit:int option -> string t

  (** Passing [allocate_bigstring] to Zstd functions will cause them to allocate an ocaml
      string to contain their result. *)
  val allocate_bigstring : size_limit:int option -> Bigstring.t t
end

module Input : sig
  (** Zstd exposes multiple API flavors which can be used to transform strings into
      strings. The [t] type encodes the various ways to pass a string from the OCaml world
      to ZStd functions. *)
  type t

  (** [from_string ?pos ?len s] will pass the content of [s] to Zstd functions. This
      incurs a copy of the OCaml string when [from_string] is called. *)
  val from_string : ?pos:int -> ?len:int -> string -> t

  (** [from_bytes ?pos ?len s] will pass the content of [s] to Zstd functions. This incurs
      an allocation of a buffer of size [len] when calling [from_bytes], and a copy of the
      ocaml bytes each time the resulting [t] is used by a compression / decompression
      function. *)
  val from_bytes : ?pos:int -> ?len:int -> Bytes.t -> t

  (** [from_bigstring ?pos ?len s] will pass the content of [s] between [pos] and
      [pos+len] to Zstd functions. This does not incur a copy. *)
  val from_bigstring : ?pos:int -> ?len:int -> Bigstring.t -> t

  (** [from_iobuf iobuf] will pass the content of [iobuf] to Zstd functions. This
      does not incur a copy. *)
  val from_iobuf : ([> read ], _) Iobuf.t -> t
end

(** Returns the decompressed size of a message. Since decompressed size is an optional
    field of compressed message, it might raise [Content_size_unknown],
    [Content_size_error] or [Error]. *)
val decompressed_size : Input.t -> Int64.t

module Compression_context : sig
  type t

  val create : unit -> t

  (** [free t] deallocate the underlying datastructure. Subsequent uses of [t] will
      cause exceptions. *)
  val free : t -> unit
end

module Decompression_context : sig
  type t

  val create : unit -> t

  (** [free t] deallocate the underlying datastructure. Subsequent uses of [t] will
      cause exceptions. *)
  val free : t -> unit
end

module Simple : sig
  (** Compresses [input] as a single frame. Raises an [Error] in case of failure. *)
  val compress : compression_level:int -> input:Input.t -> output:'a Output.t -> 'a

  (**  [input] should contain an exact number of frames.
       Raises [Error] in case of failure. *)
  val decompress : input:Input.t -> output:'a Output.t -> 'a
end

module With_explicit_context : sig
  (** This module implements compression and decompression with explicitly managed
      contexts. When compressing or decompressing many times, it is recommended to
      allocate a context just once, and re-use it for each successive compression
      operation. This will make workload friendlier for system's memory. *)

  (** Compresses [input] as a single frame. Raises an [Error] in case of failure. *)
  val compress
    :  Compression_context.t
    -> compression_level:int
    -> input:Input.t
    -> output:'a Output.t
    -> 'a

  (** [input] should contain an exact number of frames.
      Raises [Error] in case of failure. *)
  val decompress : Decompression_context.t -> input:Input.t -> output:'a Output.t -> 'a
end

module Dictionary : sig
  module Training_algorithm : sig
    module Cover : sig
      type t =
        { k : int (** Segment size : constraint: 0 < k : Reasonable range [16, 2048+] *)
        ; d : int (** dmer size : constraint: 0 < d <= k : Reasonable range [6, 16] *)
        ; steps : int
        (** Number of steps : Only used for optimization : 0 means default (40) : Higher
            means more parameters checked *)
        ; nb_threads : int
        (** Number of threads : constraint: 0 < nbThreads : 1 means single-threaded : Only
            used for optimization : Ignored if ZSTD_MULTITHREAD is not defined *)
        ; split_point : float
        (** Percentage of samples used for training: Only used for optimization : the first
            nbSamples * splitPoint samples will be used to training, the last nbSamples *
            (1 - splitPoint) samples will be used for testing, 0 means default (1.0), 1.0
            when all samples are used for both training and testing *)
        }

      (** Some default, reasonable, value for the parameters *)
      val default : t
    end

    module Fast_cover : sig
      type t =
        { k : int (** Segment size : constraint: 0 < k : Reasonable range [16, 2048+] *)
        ; d : int (** dmer size : constraint: 0 < d <= k : Reasonable range [6, 16] *)
        ; f : int
        (** log of size of frequency array : constraint: 0 < f <= 31 : 1 means default(20)*)
        ; steps : int
        (** Number of steps : Only used for optimization : 0 means default (40) : Higher
            means more parameters checked *)
        ; nb_threads : int
        (** Number of threads : constraint: 0 < nbThreads : 1 means single-threaded : Only
            used for optimization : Ignored if ZSTD_MULTITHREAD is not defined *)
        ; split_point : float
        (** Percentage of samples used for training: Only used for optimization : the first
            nbSamples * splitPoint samples will be used to training, the last nbSamples *
            (1 - splitPoint) samples will be used for testing, 0 means default (0.75), 1.0
            when all samples are used for both training and testing *)
        ; accel : int
        (** Acceleration level: constraint: 0 < accel <= 10, higher means faster and less
            accurate, 0 means default(1) *)
        }
    end

    type t =
      | Default
      (** Invokes the fast cover algorithm with reasonable default parameters. Versions of
          zstd <= 1.3.5 used the cover algorithm.*)
      | Cover of Cover.t (** Slower, higher quality generator. *)
      | Fast_cover of Fast_cover.t
      (** The new builder, named fastcover, is about 10x faster than the previous default
          generator, cover, while suffering only negligible accuracy losses (<1%). It's
          effectively an approximative version of cover, which throws away accuracy for the
          benefit of speed and memory. This is zstd's default. Slower but higher quality
          generator remains accessible using [Cover]. *)
  end

  (** [train ?dict_size strings] trains a dictionary from an array of samples. [dict_size]
      defaults to 100KB, which is a reasonable dictionary size. In general it's
      recommended to provide a few thousands samples (though this can vary a lot); and
      it's recommended that the total size of the samples should be around 100x times the
      target dictionary size.

      The underlying C function can raise, in which case this function will raise [Error s].
  *)
  val train
    :  ?dict_size:int
    -> ?training_algorithm:Training_algorithm.t
    -> string array
    -> 'a Output.t
    -> 'a

end

module Simple_dictionary : sig
  val compress
    :  Compression_context.t
    -> compression_level:int
    -> dictionary:Input.t
    -> input:Input.t
    -> output:'a Output.t
    -> 'a

  val decompress
    :  Decompression_context.t
    -> dictionary:Input.t
    -> input:Input.t
    -> output:'a Output.t
    -> 'a
end

module Bulk_processing_dictionary : sig
  module Compression : sig
    type t

    val create : dictionary:Input.t -> compression_level:int -> t

    (** [free t] deallocate the underlying datastructure. Subsequent uses of [t] will
        cause exceptions. *)
    val free : t -> unit

    val compress
      :  t
      -> context:Compression_context.t
      -> input:Input.t
      -> output:'a Output.t
      -> 'a
  end

  module Decompression : sig
    type t

    val create : dictionary:Input.t -> t

    (** [free t] deallocate the underlying datastructure. Subsequent uses of [t] will
        cause exceptions. *)
    val free : t -> unit

    val decompress
      :  t
      -> context:Decompression_context.t
      -> input:Input.t
      -> output:'a Output.t
      -> 'a
  end
end

module Streaming : sig
  (** The functions exposed in this module differ from the rest of the library, and
      operates on stream of data (rather than on single messages). Therefore, the API is
      more complex, and does not use the [Input] module.

      [compress] and [decompress] consume their inputs and add (compressed/decompressed)
      data in the output buffers. It might be necessary to call compress / decompress
      multiple times to consume the entire input buffer. Note that if there is not enough
      space left in the output buffer, calling compress / decompress will not consume any
      input. It's the duty of the caller to check if the input has been entirely consumed.
  *)

  module Compression : sig
    type t

    val create : int -> t

    val compress
      :  t
      -> inbuf:Bigstring.t
      -> inpos:int
      -> inlen:int
      -> outbuf:Bigstring.t
      -> outpos:int
      -> outlen:int
      -> int * int

    (** Attempts to flush the contents of the internal buffers of [t]. If the output size
        is too small, a single invocation of the function might not be sufficient to flush
        the buffer. *)
    val flush : t -> outbuf:Bigstring.t -> outpos:int -> outlen:int -> int * int

    (** Performs a flush and write an epilogue for the stream. The epilogue is required
        for decoders to consider the message complete. *)
    val endstream : t -> outbuf:Bigstring.t -> outpos:int -> outlen:int -> int * int

    (** [free t] deallocate the underlying datastructure. Subsequent uses of [t] will
        cause exceptions. *)
    val free : t -> unit
  end

  module Decompression : sig
    type t

    val create : unit -> t

    val decompress
      :  t
      -> inbuf:Bigstring.t
      -> inpos:int
      -> inlen:int
      -> outbuf:Bigstring.t
      -> outpos:int
      -> outlen:int
      -> int * int

    (** [free t] deallocate the underlying datastructure. Subsequent uses of [t] will
        cause exceptions. *)
    val free : t -> unit
  end
end
