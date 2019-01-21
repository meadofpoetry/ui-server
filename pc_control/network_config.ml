let (>>=) v f =
  match v with
  | None -> None
  | Some x -> f x

let result_to_opt = function
  | Ok v -> Some v
  | _ -> None

type v4 = Ipaddr.V4.t

let v4_to_yojson v = `String (Ipaddr.V4.to_string v)

let v4_of_yojson = function
  | `String v -> begin
      match Ipaddr.V4.of_string v with
      | Error (`Msg m) -> Error ("ip parsing failure: " ^ m)
      | Ok _ as ip -> ip
    end
  | _ -> Error "string expected"

let equal_v4 l r =
  (Ipaddr.V4.compare l r) = 0

type meth = Auto
          | Manual
[@@deriving yojson, eq]
let meth_to_string = function
  | Auto -> "auto"
  | Manual -> "manual"
let meth_of_string = function
  | "auto" -> Some Auto
  | "manual" -> Some Manual
  | _ -> None

type autoconnect  = False | True of priority
and priority      = int [@@deriving yojson, eq]

type address = v4 * int32 [@@deriving yojson, eq]

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

module Macaddr = struct
  include Macaddr
  let equal l r = Macaddr.compare l r = 0
  let to_yojson x = `String (Macaddr.to_string x)
  let of_yojson = function
    | `String s -> (match Macaddr.of_string s with
                    | Ok _ as m -> m
                    | Error (`Msg m) -> Error ("bad mac: " ^ m))
    | x         -> Error ("not a mac addr: " ^ (Yojson.Safe.to_string x))
end
       
type routes = { static  : address list
              ; gateway : v4 option
              } [@@deriving yojson, eq]
                  
type t            = { ethernet   : ethernet_conf
                    ; connection : conn_conf
                    ; ipv4       : ipv4_conf
                    ; ipv6       : ipv6_conf
                    ; proxy      : proxy_conf
                    } [@@deriving yojson, eq]
and ethernet_conf = { mac_address  : Macaddr.t } (* TODO make optional *)
and conn_conf     = { autoconnect  : autoconnect
                    ; id           : string
                    ; uuid         : string
                    }
and ipv4_conf     = { address : address
                    ; routes  : routes
                    ; dns     : v4 list
                    ; meth    : meth
                    }
and ipv6_conf     = unit
and proxy_conf    = unit
        
let dump x =
  to_yojson x
  |> Yojson.Safe.to_string

let restore x =
  Yojson.Safe.from_string x
  |> of_yojson
