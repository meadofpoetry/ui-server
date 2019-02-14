type cyusb_handle

type t = { handle    : cyusb_handle
         ; max_in    : int
         ; max_out   : int
         ; vendor    : int
         ; product   : int
         ; busnumber : int
         ; devaddr   : int
         }

external connected_num : unit -> int = "caml_cyusb_conn_num"

external create : int -> t = "caml_cyusb_open"

external close : unit -> unit = "caml_cyusb_close"

external test_fun : int ->
                    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  = "caml_test_fun"
