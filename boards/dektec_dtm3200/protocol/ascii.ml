open Netlib

module type Getter = sig
  type t

  val get : Cstruct.t -> t option

  val get_exn : Cstruct.t -> t
end

module type P = sig
  type t

  val of_string : string -> t
end

module Make_int (M : P) : Getter with type t = M.t = struct
  type t = M.t

  let get_exn (b : Cstruct.t) : t =
    let s = "0x" ^ Cstruct.to_string b in
    M.of_string s

  let get (b : Cstruct.t) : t option =
    try Some (get_exn b) with Failure _ -> None
end

module Int : Getter with type t = int = Make_int (struct
  type t = int

  let of_string = int_of_string
end)

module Int32 : Getter with type t = int32 = Make_int (Int32)

module Int64 : Getter with type t = int64 = Make_int (Int64)

module Ipaddr : Getter with type t = Ipaddr.V4.t = struct
  type t = Ipaddr.V4.t

  let get b =
    match Int32.get b with
    | None -> None
    | Some x -> Some (Ipaddr.V4.of_int32 x)

  let get_exn b = Ipaddr.V4.of_int32 (Int32.get_exn b)
end

module Bool : Getter with type t = bool = struct
  type t = bool

  let get b =
    match Int.get b with
    | None -> None
    | Some 0 -> Some false
    | Some 1 -> Some true
    | Some _ -> None

  let get_exn b =
    match Int.get_exn b with
    | 0 -> false
    | 1 -> true
    | x -> failwith @@ Printf.sprintf "bool parser: bad value (%d)" x
end
