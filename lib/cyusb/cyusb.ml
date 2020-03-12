type cyusb_handle

type t = {
  handle : cyusb_handle;
  max_in : int;
  max_out : int;
  vendor : int;
  product : int;
  busnumber : int;
  devaddr : int;
}

type buf =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external create : unit -> t = "caml_cyusb_open"
(** raises Failure on error *)

external close : t -> unit = "caml_cyusb_close"

external send : t -> buf -> unit = "caml_cyusb_send"
(** raises Failure on error *)

external recv : t -> buf = "caml_cyusb_recv"
(** raises Failure on error *)
