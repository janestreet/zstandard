open Ctypes

module C (F : Cstubs.FOREIGN) = struct
  open F

  let const x = x

  let compress =
    foreign
      "ZSTD_compress"
      (ptr void (* dst *)
       @-> size_t (* dstCapacity *)
       @-> const (ptr void) (* src *)
       @-> size_t (* srcSize *)
       @-> int (* compressionLevel *)
       @-> returning size_t)
  ;;

  let decompress =
    foreign
      "ZSTD_decompress"
      (ptr void (* dst *)
       @-> size_t (* dstCapacity *)
       @-> const (ptr void) (* src *)
       @-> size_t (* compressedSize *)
       @-> returning size_t)
  ;;

  (* #define ZSTD_CONTENTSIZE_UNKNOWN (0ULL - 1) *)
  (* #define ZSTD_CONTENTSIZE_ERROR (0ULL - 2) *)

  let getFrameContentSize =
    foreign
      "ZSTD_getFrameContentSize"
      (const (ptr void) (* src *) @-> size_t (* srcSize *) @-> returning Ctypes.ullong)
  ;;

  let compressBound =
    foreign "ZSTD_compressBound" (size_t (* srcSize *) @-> returning size_t)
  ;;

  let isError = foreign "ZSTD_isError" (size_t (* code *) @-> returning uint)

  let getErrorName =
    foreign "ZSTD_getErrorName" (size_t (* code *) @-> returning (const (ptr char)))
  ;;

  let maxCLevel = foreign "ZSTD_maxCLevel" (void @-> returning int)

  module Context = struct
    module Compression = struct
      type s
      type t = s structure (* will be abstracted in the mli *)

      let t : t typ = typedef (structure "ZSTD_CCtx_s") "ZSTD_CCtx"
      let create = foreign "ZSTD_createCCtx" (void @-> returning (ptr t))
      let free = foreign "ZSTD_freeCCtx" (ptr t @-> returning size_t)

      let compress =
        foreign
          "ZSTD_compressCCtx"
          (ptr t
           @-> ptr void (* dst *)
           @-> size_t (* dstCapacity *)
           @-> const (ptr void) (* src *)
           @-> size_t (* srcSize *)
           @-> int (* compressionLevel *)
           @-> returning size_t)
      ;;
    end

    module Decompression = struct
      type s
      type t = s structure (* will be abstracted in the mli *)

      let t : t typ = typedef (structure "ZSTD_DCtx_s") "ZSTD_DCtx"
      let create = foreign "ZSTD_createDCtx" (void @-> returning (ptr t))
      let free = foreign "ZSTD_freeDCtx" (ptr t @-> returning size_t)

      let decompress =
        foreign
          "ZSTD_decompressDCtx"
          (ptr t
           @-> ptr void (* dst *)
           @-> size_t (* dstCapacity *)
           @-> const (ptr void) (* src *)
           @-> size_t (* srcSize *)
           @-> returning size_t)
      ;;
    end
  end

  module Simple_dictionary = struct
    let compress_usingDict =
      foreign
        "ZSTD_compress_usingDict"
        (ptr Context.Compression.t (* ctx *)
         @-> ptr void (* dst *)
         @-> size_t (* dstCapacity *)
         @-> const (ptr void) (* src *)
         @-> size_t (* srcSize *)
         @-> const (ptr void) (* dict *)
         @-> size_t (* dictSize *)
         @-> int (* compressionLevel *)
         @-> returning size_t)
    ;;

    let decompress_usingDict =
      foreign
        "ZSTD_decompress_usingDict"
        (ptr Context.Decompression.t (* dctx *)
         @-> ptr void (* dst *)
         @-> size_t (* dstCapacity *)
         @-> const (ptr void) (* src *)
         @-> size_t (* srcSize *)
         @-> const (ptr void) (* dict *)
         @-> size_t (* dictSize *)
         @-> returning size_t)
    ;;
  end

  module Bulk_processing_dictionary = struct
    module Compression = struct
      type s
      type t = s structure

      let t : t typ = typedef (structure "ZSTD_CDict_s") "ZSTD_CDict"

      let create =
        foreign
          "ZSTD_createCDict"
          (const (ptr void) (* dictBuffer *)
           @-> size_t (* dictSize *)
           @-> int (* compressionLevel *)
           @-> returning (ptr t))
      ;;

      let free = foreign "ZSTD_freeCDict" (ptr t @-> returning size_t)

      let compress =
        foreign
          "ZSTD_compress_usingCDict"
          (ptr Context.Compression.t
           @-> ptr void (* dst *)
           @-> size_t (* dstCapacity *)
           @-> const (ptr void) (* src *)
           @-> size_t (* srcSize *)
           @-> const (ptr t)
           @-> returning size_t)
      ;;
    end

    module Decompression = struct
      type s
      type t = s structure

      let t : t typ = typedef (structure "ZSTD_DDict_s") "ZSTD_DDict"

      let create =
        foreign
          "ZSTD_createDDict"
          (const (ptr void) (* dictBuffer *)
           @-> size_t (* dictSize *)
           @-> returning (ptr t))
      ;;

      let free = foreign "ZSTD_freeDDict" (ptr t @-> returning size_t)

      let decompress =
        foreign
          "ZSTD_decompress_usingDDict"
          (ptr Context.Decompression.t
           @-> ptr void (* dst *)
           @-> size_t (* dstCapacity *)
           @-> const (ptr void) (* src *)
           @-> size_t (* srcSize *)
           @-> const (ptr t)
           @-> returning size_t)
      ;;
    end
  end

  module Streaming = struct
    let inbuffer : [ `Inbuffer ] structure typ = structure "ZSTD_inBuffer_s"
    let inbuf_psrc = field inbuffer "src" (ptr void)
    let inbuf_size = field inbuffer "size" size_t
    let inbuf_pos = field inbuffer "pos" size_t
    let () = seal inbuffer
    let outbuffer : [ `Outbuffer ] structure typ = structure "ZSTD_outBuffer_s"
    let outbuf_pdst = field outbuffer "dst" (ptr void)
    let outbuf_size = field outbuffer "size" size_t
    let outbuf_pos = field outbuffer "pos" size_t
    let () = seal outbuffer
    let cctx = Context.Compression.t
    let dctx = Context.Decompression.t

    module Compression = struct
      let create = foreign "ZSTD_createCStream" (void @-> returning (ptr cctx))
      let free = foreign "ZSTD_freeCStream" (ptr cctx @-> returning size_t)
      let init = foreign "ZSTD_initCStream" (ptr cctx @-> int @-> returning size_t)

      let compress =
        foreign
          "ZSTD_compressStream"
          (ptr cctx @-> ptr outbuffer @-> ptr inbuffer @-> returning size_t)
      ;;

      let flushStream =
        foreign "ZSTD_flushStream" (ptr cctx @-> ptr outbuffer @-> returning size_t)
      ;;

      let endStream =
        foreign "ZSTD_endStream" (ptr cctx @-> ptr outbuffer @-> returning size_t)
      ;;

      let inbuf_size_hint = foreign "ZSTD_CStreamInSize" (void @-> returning size_t)
      let outbuf_size_hint = foreign "ZSTD_CStreamOutSize" (void @-> returning size_t)
    end

    module Decompression = struct
      let create = foreign "ZSTD_createDStream" (void @-> returning (ptr dctx))
      let free = foreign "ZSTD_freeDStream" (ptr dctx @-> returning size_t)
      let init = foreign "ZSTD_initDStream" (ptr dctx @-> returning size_t)

      let decompress =
        foreign
          "ZSTD_decompressStream"
          (ptr dctx @-> ptr outbuffer @-> ptr inbuffer @-> returning size_t)
      ;;

      let inbuf_size_hint = foreign "ZSTD_DStreamInSize" (void @-> returning size_t)
      let outbuf_size_hint = foreign "ZSTD_DStreamOutSize" (void @-> returning size_t)
    end
  end

  module Dictionary = struct
    module Cover_params = struct
      let s : [ `Cover ] structure typ = structure "ZDICT_cover_params_s"
      let k = field s "k" uint
      let d = field s "d" uint
      let steps = field s "steps" uint
      let nbThreads = field s "nbThreads" uint
      let splitPoint = field s "splitPoint" double
      let () = seal s
      let t = typedef s "ZDICT_cover_params_t"
    end

    module FastCover_params = struct
      let s : [ `fastCover ] structure typ = structure "ZDICT_fastCover_params_s"
      let k = field s "k" uint
      let d = field s "d" uint
      let f = field s "f" uint
      let steps = field s "steps" uint
      let nbThreads = field s "nbThreads" uint
      let splitPoint = field s "splitPoint" double
      let accel = field s "accel" uint
      let () = seal s
      let t = typedef s "ZDICT_fastCover_params_t"
    end

    let trainFromBuffer =
      foreign
        "ZDICT_trainFromBuffer"
        (ptr void (* dictBuffer *)
         @-> size_t (* dictBufferCapacity *)
         @-> const (ptr void) (* samplesBuffer *)
         @-> const (ptr size_t) (* samplesSizes *)
         @-> uint (* nbSamples *)
         @-> returning size_t)
    ;;

    let trainFromBuffer_cover =
      foreign
        "ZDICT_trainFromBuffer_cover"
        (ptr void (* dictBuffer *)
         @-> size_t (* dictBufferCapacity *)
         @-> const (ptr void) (* samplesBuffer *)
         @-> const (ptr size_t) (* samplesSizes *)
         @-> uint (* nbSamples *)
         @-> Cover_params.t (* parameters *)
         @-> returning size_t)
    ;;

    let trainFromBuffer_fastCover =
      foreign
        "ZDICT_trainFromBuffer_fastCover"
        (ptr void (* dictBuffer *)
         @-> size_t (* dictBufferCapacity *)
         @-> const (ptr void) (* samplesBuffer *)
         @-> const (ptr size_t) (* samplesSizes *)
         @-> uint (* nbSamples *)
         @-> FastCover_params.t (* parameters *)
         @-> returning size_t)
    ;;

    let isError = foreign "ZDICT_isError" (size_t (* code *) @-> returning uint)

    let getErrorName =
      foreign "ZDICT_getErrorName" (size_t (* code *) @-> returning (const (ptr char)))
    ;;
  end
end
