open Netlib

let (>>=) v f =
  match v with
  | None -> None
  | Some x -> f x

let result_to_opt = function
  | Ok v -> Some v
  | _ -> None

type meth =
  | Auto
  | Manual [@@deriving yojson, eq]

let meth_to_string = function
  | Auto -> "auto"
  | Manual -> "manual"

let meth_of_string = function
  | "auto" -> Some Auto
  | "manual" -> Some Manual
  | _ -> None

type autoconnect = False | True of priority
and priority     = int [@@deriving yojson, eq]

type address = Ipaddr.V4.t * int32 [@@deriving yojson, eq]

let address_to_string (a,m) =
  let a, m = (Ipaddr.V4.to_string a), (Int32.to_string m) in
  a ^ "/" ^ m

let address_of_string s =
  match String.split_on_char '/' s with
  | [a;m] -> Ipaddr.V4.of_string a
             |> result_to_opt
             >>= fun a ->
             Int32.of_string_opt m
             >>= fun m ->
             Some (a,m)
  | _ -> None

type routes =
  { static  : address list
  ; gateway : Ipaddr.V4.t option
  } [@@deriving yojson, eq]

type t =
  { ethernet   : ethernet_conf
  ; connection : conn_conf
  ; ipv4       : ipv4_conf
  ; ipv6       : ipv6_conf
  ; proxy      : proxy_conf
  } [@@deriving yojson, eq]
and ethernet_conf =
  { mac_address : Macaddr.t
  } (* TODO make optional *)
and conn_conf =
  { autoconnect  : autoconnect
  ; id           : string
  ; uuid         : string
  }
and ipv4_conf =
  { address : address
  ; routes  : routes
  ; dns     : Ipaddr.V4.t list
  ; meth    : meth
  }
and ipv6_conf = unit
and proxy_conf = unit

let to_string x =
  to_yojson x
  |> Yojson.Safe.to_string

let of_string x =
  Yojson.Safe.from_string x
  |> of_yojson
  |> function Ok x -> x | Error e -> failwith e
