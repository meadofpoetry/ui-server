open Topology

let (>>=) m f = match m with
  | None -> None
  | Some v -> f v
   
(** Main stream ID *)
module ID : sig

  type t
  type api_fmt = t
  val to_string : t -> string
  val of_string_opt : string -> t option
  val of_string : string -> t
  val to_yojson : t -> Yojson.Safe.json
  val of_yojson : Yojson.Safe.json -> (t, string) result
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val make : string -> t
  val typ : string
  val fmt : api_fmt Netlib.Uri.Path.Format.fmt

end = struct
  (* TODO remove in 4.08 *)
  let get_exn = function Some v -> v | None -> failwith "None"

  let of_opt = function Some v -> Ok v | None -> Error "None"
                                             
  include Uuidm

  type api_fmt = t

  let typ = "uuid"

  let fmt = Netlib.Uri.Path.Format.Uuid

  let to_string (x : t) = to_string x
  let of_string_opt (s : string) = of_string s
  let of_string (s : string) = get_exn @@ of_string_opt s

  let to_yojson (x : t) : Yojson.Safe.json =
    `String (Uuidm.to_string x)
  let of_yojson : Yojson.Safe.json -> (t, string) result = function
    | `String s -> of_opt @@ Uuidm.of_string s
    | _ -> Error "uuid_of_yojson: not a string"

  let make (s : string) =
    v5 ns_url s

end

(** Stream source description/parameters *)
module Source = struct

  let round_freq (x : int64) =
    let ( mod ), ( / ), (=) = Int64.(rem, div, equal) in
    if x mod 1_000_000_000L = 0L then x / 1_000_000_000L, "ГГц"
    else if x mod 1_000_000L = 0L then x / 1_000_000L, "МГц"
    else if x mod 1_000L = 0L then x / 1_000L, "кГц"
    else x, "Гц"

  (** DVB-T2 source description *)
  type dvb_t2 =
    { freq : int64
    ; plp : int
    ; bw : float
    } [@@deriving yojson, show, eq, ord]

  (** DVB-T source description *)
  type dvb_t =
    { freq : int64
    ; bw : float
    } [@@deriving yojson, show, eq, ord]

  (** DVB-C source description *)
  type dvb_c = dvb_t [@@deriving yojson, show, eq, ord]

  (** T2-MI source description *)
  type t2mi =
    { stream_id : int
    ; plp : int
    } [@@deriving yojson, show, eq, ord]

  (** IP v4 source description *)
  type ipv4 =
    { scheme : string
    ; addr : Netlib.Ipaddr.V4.t
    ; port : int
    } [@@deriving yojson, show, eq, ord]

  (** Source desciption type *)
  type t =
    | DVB_T2 of dvb_t2
    | DVB_T of dvb_t
    | DVB_C of dvb_c
    | IPV4 of ipv4
    | ASI
    | SPI
    | T2MI of t2mi [@@deriving yojson, show, eq, ord]

  let dvb_t2_to_string (x : dvb_t2) =
    let open Printf in
    let freq, unit = round_freq x.freq in
    let bw = sprintf "полоса %g МГц" x.bw in
    sprintf "DVB-T2, %Lu %s, %s, PLP %d" freq unit bw x.plp

  let dvb_t_to_string (x : dvb_t) =
    let open Printf in
    let freq, unit = round_freq x.freq in
    let bw = sprintf "полоса %g МГц" x.bw in
    sprintf "DVB-T, %Lu %s, %s" freq unit bw

  let dvb_c_to_string (x : dvb_c) =
    dvb_t_to_string x

  let asi_to_string () =
    "ASI"

  let spi_to_string () =
    "SPI"

  let t2mi_to_string (x : t2mi) =
    let open Printf in
    sprintf "T2-MI PLP. Stream ID %d, PLP %d" x.stream_id x.plp

  let ipv4_to_string (x : ipv4) =
    Uri.make
      ~scheme:x.scheme
      ~host:(Netlib.Ipaddr.V4.to_string x.addr)
      ~port:x.port
      ()
    |> Uri.to_string

  let to_string = function
    | DVB_T2 x -> dvb_t2_to_string x
    | DVB_T x -> dvb_t_to_string x
    | DVB_C x -> dvb_c_to_string x
    | T2MI x -> t2mi_to_string x
    | IPV4 x -> ipv4_to_string x
    | ASI -> asi_to_string ()
    | SPI -> spi_to_string ()

end

(** Multi TS ID *)
module Multi_TS_ID : sig

  (** Pure multi TS ID format.
      Pure format is usually used in exchange protocol messages because
      it is easier to parse then the raw format.

      | [31:28] |   [27:8]   |  [7:0]   |
      |---------+------------+----------|
      |  rfu    |  num[19:0] | src[7:0] |

      Raw multi TS ID format.
      Raw format is used when ID is transmitted in TS stream.
      Constants in this formats guarantee that inner bytes will never
      take the value 0x47 (sync byte in MPEG-TS).


      | [31] |   [30:23]  | [22] |  [21:10]  | [9] |   [8:1]   | [0] |
      |------+------------+------+-----------+-----+-----------+-----|
      |  1   | num[19:12] |  0   | num[11:0] |  0  |  src[7:0] |  0  |

   *)

  type t

  val to_yojson : t -> Yojson.Safe.json
  val of_yojson : Yojson.Safe.json -> (t, string) result
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val source_id : t -> int
  val stream_id : t -> int
  val make : source_id:int -> stream_id:int -> t
  val of_int32_raw : int32 -> t
  val of_int32_pure : int32 -> t
  val to_int32_raw : t -> int32
  val to_int32_pure : t -> int32

end = struct

  [@@@ocaml.warning "-32"]
  type t =
    | Parsed of parsed
    | Raw of int32
    | Pure of int32
  and parsed =
    { source_id : int
    ; stream_id : int
    } [@@deriving eq, ord]
  [@@@ocaml.warning "+32"]

  let parse_pure (i : int32) : parsed =
    let open Int32 in
    let src = to_int @@ logand i 0xFFl in
    let num = to_int @@ shift_right_logical (logand i 0xFFFFF00l) 8 in
    { source_id = src
    ; stream_id = num
    }

  let parse_raw (i : int32) : parsed =
    let open Int32 in
    let src = to_int @@ shift_right_logical (logand i 0x1FEl) 1 in
    let num1 = shift_right_logical (logand i 0x7F800000l) 11 in
    let num2 = shift_right_logical (logand i 0x3FFC00l) 10 in
    let num = to_int @@ logor num1 num2 in
    { source_id = src
    ; stream_id = num
    }

  let make_pure (p : parsed) : int32 =
    let open Int32 in
    let src = Int32.of_int p.source_id in
    let num = Int32.of_int p.stream_id in
    logor (shift_left num 8) src

  let make_raw (p : parsed) : int32 =
    let open Int32 in
    let src = shift_left (Int32.of_int p.source_id) 1 in
    let num = Int32.of_int p.stream_id in
    let num1 = shift_left (logand num 0xFF000l) 11 in
    let num2 = shift_left (logand num 0xFFFl) 10 in
    (logor (logor num1 num2) src)
    |> (logor) 0x80000000l   (* ensure ones at right places *)
    |> (logand) 0xFFBFFDFEl  (* ensure zeros at right places *)

  let make ~source_id ~stream_id : t =
    Parsed { source_id; stream_id }

  let parse = function
    | Parsed x -> x
    | Raw i -> parse_raw i
    | Pure i -> parse_pure i

  let compare (x : t) (y : t) = match x, y with
    | Raw x, Raw y -> Int32.compare x y
    | Pure x, Pure y -> Int32.compare x y
    | x, y -> compare_parsed (parse x) (parse y)

  let equal x y = 0 = compare x y

  let of_int32_raw (i : int32) = Raw i

  let of_int32_pure (i : int32) = Pure i

  let to_int32_raw = function
    | Parsed x -> make_raw x
    | Raw i -> i
    | Pure i -> parse_pure i |> make_raw

  let to_int32_pure = function
    | Parsed x -> make_pure x
    | Pure i -> i
    | Raw i -> parse_raw i |> make_pure

  let pp ppf t =
    let p = parse t in
    Format.fprintf ppf "{ source_id = %d; stream_id = %d }"
      p.source_id p.stream_id

  let show t = Format.asprintf "%a" pp t

  let source_id (t : t) = (parse t).source_id

  let stream_id (t : t) = (parse t).stream_id

  let to_yojson (t : t) =
    Util_json.Int32.to_yojson (to_int32_pure t)

  let of_yojson (json : Yojson.Safe.json) =
    match Util_json.Int32.of_yojson json with
    | Error _ as e -> e
    | Ok v -> Ok (of_int32_pure v)

end

type stream_type =
  | TS
  | T2MI [@@deriving yojson, eq, show, ord]

type tsoip_id =
  { addr : Netlib.Ipaddr.V4.t
  ; port : int
  } [@@deriving yojson, eq, show, ord]

type container_id =
  | TS_raw
  | TS_multi of Multi_TS_ID.t
  | TSoIP of tsoip_id [@@deriving yojson, eq, show, ord]

           
let tsoip_id_of_uri (x : Netlib.Uri.t) : tsoip_id option =
  Netlib.Uri.path_v4 x >>= fun addr ->
  Netlib.Uri.port x >>= fun port ->
  Some { addr; port }

module Raw = struct

  type t =
    { source : source
    ; typ : stream_type
    ; id : container_id
    }
  and source_node =
    | Port of int
    | Board
    | Stream of container_id
  and source =
    { node : source_node
    ; info : Source.t
    } [@@deriving yojson, eq, show]

end

type t =
  (* stream source node and description *)
  { source : source
  (* unique stream ID across the system *)
  ; id : ID.t
  (* stream type *)
  ; typ : stream_type
  (* original container id *)
  ; orig_id : container_id
  }
and source_node =
  | Entry of topo_entry (* stream from input or generated by a board*)
  | Stream of t         (* stream extracted from another stream *)
and source =
  { node : source_node (* source node *)
  ; info : Source.t    (* details about stream source *)
  } [@@deriving yojson, eq, show, ord]

let make_id (src : source) : ID.t =
  let node = match src.node with
    | Entry (Input i) -> Printf.sprintf "%d/%d" (input_to_enum i.input) i.id
    | Entry (Board b) -> string_of_int b.control
    | Stream s -> ID.to_string s.id in
  let info = match src.info with
    | DVB_T2 x -> "dvbt2/" ^ Source.dvb_t2_to_string x
    | DVB_T x -> "dvbt/" ^ Source.dvb_t_to_string  x
    | DVB_C x -> "dvbc/" ^ Source.dvb_c_to_string  x
    | ASI -> "asi"
    | SPI -> "spi"
    | T2MI x -> "t2mi/" ^ Source.t2mi_to_string x
    | IPV4 x -> "ipv4/" ^ Source.ipv4_to_string x in
  ID.make (node ^ "/" ^ info)

let to_multi_id (t : t) : Multi_TS_ID.t =
  match t.orig_id with
  | TS_multi x -> x
  | _ -> failwith "not a multi TS"

let typ_to_string = function
  | TS -> "ts"
  | T2MI -> "t2mi"
let typ_of_string = function
  | "ts" -> TS
  | "t2mi" -> T2MI
  | _ -> failwith "bad typ string"

let equal l r = ID.equal l.id r.id

let compare l r =
  if equal l r then 0
  else compare l r

let find_by_multi_id (id : Multi_TS_ID.t)
      (streams : t list) =
  List.find_opt (fun (s : t) ->
      match s.orig_id with
      | TS_multi x -> Multi_TS_ID.equal x id
      | _ -> false) streams

let find_by_id (id : ID.t) (streams : t list) =
  List.find_opt (fun (s : t) -> ID.equal s.id id) streams

let rec get_input (s : t) : topo_input option =
  match s.source.node with
  | Stream s -> get_input s
  | Entry Input i -> Some i
  | Entry Board _ -> None

let to_topo_port (ports : topo_port list) (t : t) : topo_port option =
  let rec get_port input = function
    | [] -> None
    | hd :: tl ->
       begin match hd.child with
       | Input x ->
          if equal_topo_input x input
          then Some hd else get_port input tl
       | Board x ->
          begin match get_port input x.ports with
          | Some _ -> Some hd
          | None -> get_port input tl
          end
       end
  in
  get_input t >>= fun input ->
  get_port input ports

module Table = struct

  type url = Netlib.Uri.t [@@deriving eq]

  type source_state =
    [ `Forbidden
    | `Limited of int
    | `Unlimited
    ] [@@deriving yojson, eq]

  type set_error =
    [ `Not_in_range
    | `Limit_exceeded of (int * int)
    | `Forbidden
    | `Internal_error of string
    ] [@@deriving yojson, eq]

  type stream =
    { url : Netlib.Uri.t option (* if None - stream is not selected *)
    ; present : bool
    ; stream : t
    } [@@deriving yojson, eq, ord]

  type setting =
    { url : Netlib.Uri.t
    ; stream : t
    } [@@deriving yojson, eq, ord]

  let set_error_to_string : set_error -> string = function
    | `Not_in_range -> "Not in range"
    | `Forbidden -> "Forbidden"
    | `Internal_error e -> Printf.sprintf "Internal error: %s" e
    | `Limit_exceeded (exp, got) ->
      Printf.sprintf "Limit of %d streams exceeded (%d)" exp got

end

module Log_message = struct

  type level =
    | Info
    | Warn
    | Err
    | Fatal [@@deriving eq, enum, ord]

  let level_to_yojson l = `Int (level_to_enum l)

  let level_of_yojson = function
    | `Int i -> begin
        match level_of_enum i with
        | None -> Error "level_of_yojson: bad level"
        | Some v -> Ok v
      end
    | _ -> Error "level_of_yojson: bad level"

  type node = Board of int
            | Cpu of string [@@deriving eq, yojson]

  type t =
    { time : Time.t
    ; level : level
    ; message : string
    ; info : string
    ; node : node option
    ; input : topo_input option
    ; stream: ID.t option
    ; service : string option
    ; pid : pid option
    }
  and pid =
    { typ : string option
    ; id : int
    } [@@deriving eq, yojson, make]

  type source = [`All | `Id of ID.t list] -> t list React.event

end

type marker =
  [ `Input of Topology.input * int
  | `Board of int
  ] [@@deriving yojson, eq]

let marker_compare (l : marker) (r : marker) = match l, r with
  | (`Input (li, lid)), (`Input (ri, rid)) ->
     let c = Topology.compare_input li ri in
     if c <> 0 then c
     else Stdlib.compare lid rid
  | (`Board l), (`Board r) -> Stdlib.compare l r
  | (`Board _), (`Input _) -> -1
  | (`Input _), (`Board _) -> 1

type stream_setting =
  (marker * t list) list [@@deriving yojson, eq]

type stream_table_row =
  (marker
   * Table.source_state
   * Table.stream list) [@@deriving yojson, eq]

type stream_table =
  stream_table_row list [@@deriving yojson ,eq]

type stream_list =
  (board_num * t list) list
and board_num = int [@@deriving yojson, eq]
              
let set_error_to_string : Table.set_error -> string =
  function
  | `Not_in_range -> "Not in range"
  | `Limit_exceeded (exp,got) ->
     Printf.sprintf "Limit exceeded: got %d streams, \
                     but only %d is available" got exp
  | `Forbidden -> "Forbidden"
  | `Internal_error e -> Printf.sprintf "Internal error: %s" e
