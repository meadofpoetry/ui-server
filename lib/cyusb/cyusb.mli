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

val create : unit -> t
(** raises Failure on error *)

val close : t -> unit

val send : t -> buf -> unit
(** raises Failure on error *)

val recv : t -> buf
(** raises Failure on error *)
