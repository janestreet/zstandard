let () =
  let fmt file = Format.formatter_of_out_channel (open_out file) in
  let fmt_c = fmt "zstd_stubs.c" in
  Format.fprintf fmt_c {|#include "zstd.h"@.|};
  (* This allows us to expose the various dictionary training functions   *)
  Format.fprintf fmt_c {|#define ZDICT_STATIC_LINKING_ONLY@.|};
  Format.fprintf fmt_c {|#include "zdict.h"@.|};
  Cstubs.write_c
    ~concurrency:Cstubs.unlocked
    fmt_c
    ~prefix:"caml_"
    (module Zstd_bindings.C);
  let fmt_ml = fmt "zstd_generated.ml" in
  Cstubs.write_ml fmt_ml ~prefix:"caml_" (module Zstd_bindings.C);
  flush_all ()
;;
