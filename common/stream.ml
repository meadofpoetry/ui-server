open Topology
open Containers

module Source = struct

  let round_freq (x:int64) =
    let ( mod ), ( / ), (=) = Int64.(rem, div, equal) in
    if x mod 1_000_000_000L  = 0L then x / 1_000_000_000L, "ГГц"
    else if x mod 1_000_000L = 0L then x / 1_000_000L, "МГц"
    else if x mod 1_000L     = 0L then x / 1_000L, "кГц"
    else x, "Гц"

  (** DVB-T2 source description *)
  type dvb_t2 =
    { freq : int64
    ; plp  : int
    ; bw   : float
    } [@@deriving yojson, show, eq, ord]

  (** DVB-T source description *)
  type dvb_t =
    { freq : int64
    ; bw   : float
    } [@@deriving yojson, show, eq, ord]

  (** DVB-C source description *)
  type dvb_c = dvb_t [@@deriving yojson, show, eq, ord]

  (** T2-MI source description *)
  type t2mi =
    { stream_id : int
    ; plp       : int
    } [@@deriving yojson, show, eq, ord]

  (** IP v4 source description *)
  type ipv4 =
    { scheme : string
    ; addr   : Ipaddr_ext.V4.t
    ; port   : int
    } [@@deriving yojson, show, eq, ord]

  (** Source desciption type *)
  type t =
    | DVB_T2 of dvb_t2
    | DVB_T  of dvb_t
    | DVB_C  of dvb_c
    | IPV4   of ipv4
    | ASI
    | SPI
    | T2MI   of t2mi [@@deriving yojson, show, eq, ord]

  let dvb_t2_to_string (x:dvb_t2) =
    let open Printf in
    let freq, unit = round_freq x.freq in
    let bw = sprintf "полоса %g МГц" x.bw in
    sprintf "DVB-T2, %Lu %s, %s, PLP %d" freq unit bw x.plp

  let dvb_t_to_string (x:dvb_t) =
    let open Printf in
    let freq, unit = round_freq x.freq in
    let bw = sprintf "полоса %g МГц" x.bw in
    sprintf "DVB-T, %Lu %s, %s" freq unit bw

  let dvb_c_to_string (x:dvb_c) =
    dvb_t_to_string x

  let asi_to_string () =
    "ASI"

  let spi_to_string () =
    "SPI"

  let t2mi_to_string (x:t2mi) =
    let open Printf in
    sprintf "T2-MI Stream ID: %d, PLP %d" x.stream_id x.plp

  let ipv4_to_string (x:ipv4) =
    Uri.make
      ~scheme:x.scheme
      ~host:(Ipaddr_ext.V4.to_string x.addr)
      ~port:x.port
      ()
    |> Uri.to_string

  let to_string = function
    | DVB_T2 x -> dvb_t2_to_string x
    | DVB_T  x -> dvb_t_to_string x
    | DVB_C  x -> dvb_c_to_string x
    | ASI      -> asi_to_string ()
    | SPI      -> spi_to_string ()
    | T2MI   x -> t2mi_to_string x
    | IPV4   x -> ipv4_to_string x

end

type id =
  | Single
  | T2mi_plp of int
  | Dvb of int * int
  | Unknown of int32 [@@deriving show, eq, ord]

module Description = struct

  type base =
    { tsid : int
    ; onid : int
    }

  type dvb =
    { mode : [ `T | `T2 | `C ]
    ; freq : int
    ; plp  : int
    ; bw   : [ `Bw6 | `Bw7 | `Bw8 ]
    }

  type ip =
    { addr : Ipaddr_ext.V4.t
    ; port : int
    }

  type t2mi_plp =
    { plp_id : int
    }

  type node =
    | Dvb      of dvb
    | Ip       of ip
    | Asi
    | T2mi_plp of t2mi_plp

  type sid = [ `Multy_id of id | `Ip of Url.t | `Single ]

  type typ = [ `Ts | `T2mi ]

  type stream =
    { source      : src
    ; id          : sid
    ; typ         : typ
    ; label       : string
    ; description : node
    }
  and src = Port   of int
          | Local
          | Stream of stream

  type description = (node list) * topo_entry

  type t =
    { source      : source
    ; id          : sid
    ; typ         : typ
    ; label       : string
    ; description : description
    }
  and source = Entry  of Topology.topo_entry
             | Parent of t

end

let id_of_int32 : int32 -> id = function
  | 0l -> Single
  | x when Int32.equal (Int32.logand x (Int32.of_int 0xFFFF0000)) Int32.zero
    -> let x'     = Int32.to_int x in
       let stream = (x' land 0x0000FF00) lsr 8 in
       let plp    = (x' land 0xFF) in
       (match stream with
        | 1             -> T2mi_plp plp
        | 2 | 3 | 4 | 5 -> Dvb (stream - 2, plp)
        | _             -> Unknown x)
  | _ as x -> Unknown x

let id_to_int32 : id -> int32 = function
  | Single -> 0l
  | T2mi_plp plp ->
     1 lsl 8
     |> Int32.of_int
     |> Int32.logor (Int32.of_int plp)
  | Dvb (stream,plp) ->
     (2 + stream) lsl 8
     |> Int32.of_int
     |> Int32.logor (Int32.of_int plp)
  | Unknown x -> x

let id_to_yojson id : Yojson.Safe.json =
  let i32 = id_to_int32 id in `Intlit (Int32.to_string i32)
let id_of_yojson json : (id,string) result = match json with
  | `Intlit i -> Result.of_opt (Option.map id_of_int32 @@ Int32.of_string i)
  | `Int i    -> Ok (id_of_int32 @@ Int32.of_int i)
  | _         -> Error "not an int32"

type stream_id =
  [ `Ip of Url.t
  | `Ts of id ] [@@deriving yojson, show, eq, ord]

type stream =
  { source      : src
  ; id          : stream_id
  ; typ         : typ
  ; description : Source.t
  }
and typ = [ `Ts | `T2mi ]
and src = Port   of int
        | Stream of id [@@deriving yojson, show, eq, ord]

type t =
  { source      : source
  ; id          : stream_id
  ; typ         : typ
  ; description : Source.t
  }
and source = Input  of Topology.topo_input
           | Parent of t [@@deriving yojson, show, ord]

let typ_to_string = function
  | `Ts   -> "ts"
  | `T2mi -> "t2mi"
let typ_of_string = function
  | "ts"   -> `Ts
  | "t2mi" -> `T2mi
  | _      -> failwith "bad typ string"

let to_short_name (t:t) =
  let src = match t.source with
    | Input i  -> Topology.get_input_name i
    | Parent _ -> "Поток"
  in
  let id  = match t.id with
    | `Ip uri -> Some (Url.to_string uri)
    | _       -> None
  in
  let s = Printf.sprintf "Источник: %s" src in
  match id with
  | Some id -> s ^ ", " ^ id
  | None    -> s

let rec equal l r =
  match l.id, r.id with
  | `Ip ul, `Ip ur ->
     if Url.equal ul ur
     then if equal_source l.source r.source
          then equal_typ l.typ r.typ
          else false
     else false
  | `Ts il, `Ts ir ->
     if equal_id il ir
     then if equal_source l.source r.source
          then equal_typ l.typ r.typ
          else false
     else false
  | _ -> false
and equal_source l r = match l, r with
  | Input l, Input r -> Topology.equal_topo_input l r
  | Parent l, Parent r -> equal l r
  | _ -> false
and equal_typ l r = match l, r with
  | `Ts, `Ts     -> true
  | `T2mi, `T2mi -> true
  | _ -> false

let to_topo_port (b:topo_board) (t:t) =
  let rec get_input = function
    | Parent x -> get_input x.source
    | Input x  -> x
  in
  let input = get_input t.source in
  let rec get_port = function
    | []    -> None
    | h::tl -> (match h.child with
                | Input x -> if equal_topo_input x input then Some h else get_port tl
                | Board x -> (match get_port x.ports with
                              | Some _ -> Some h
                              | None   -> get_port tl))
  in get_port b.ports

let rec get_input s =
  match s.source with
  | Parent s -> get_input s
  | Input  i -> i
