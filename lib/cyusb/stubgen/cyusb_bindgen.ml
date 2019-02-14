
let usage () =
  Printf.printf "usage:\n\tcyusb_bindgen [-c|-ml]\n";
  exit (-1)
   
let () =
  if Array.length Sys.argv <> 2
  then usage ()
  else
    match Sys.argv.(1) with
    | "-c" ->
       let fmt = Format.formatter_of_out_channel (open_out "cyusb_stubs.c") in
       Format.fprintf fmt "#include \"cyusb_iface.h\"@.";
       Cstubs.write_c fmt
         ~concurrency:Cstubs.lwt_preemptive
         ~prefix:"caml_"
         (module Cyusb_bindings.C);
    | "-ml" ->
       let fmt = Format.formatter_of_out_channel (open_out "cyusb_generated.ml") in
       Cstubs.write_ml fmt
         ~concurrency:Cstubs.lwt_preemptive
         ~prefix:"caml_"
         (module Cyusb_bindings.C)
    | _ -> usage ()
