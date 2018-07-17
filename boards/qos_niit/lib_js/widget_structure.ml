open Containers
open Components
open Board_types.Streams.TS
open Lwt_result.Infix
open Common
open Board_types

type config =
  { stream       : Stream.id
  } [@@deriving yojson]

let default_config =
  { stream       = Single
  }

type dumpable =
  { name   : string * string
  ; get    : unit -> (string,string) Lwt_result.t
  ; prev   : string option React.signal
  }

(* Widget default name *)
let name = "Структура"

(* Settings widget *)
let settings = None

let (^::) = List.cons_maybe

let base_class = "qos-niit-structure"

let id_of_widget = fun (w:#Widget.t) ->
  w#get_attribute "data-id"
  |> Option.get_exn
  |> int_of_string

type ('a,'b) setter =
  { get : 'a -> 'b
  ; eq  : 'b -> 'b -> bool
  ; upd : 'b -> unit
  }

let setter ?(previous:'a option) (model:'a) (s:('a,'b) setter) =
  match previous with
  | Some prev ->
     if s.eq (s.get prev) (s.get model)
     then s.upd @@ s.get model
  | None -> s.upd @@ s.get model

module type Node = sig

  type model
  type widget

  val equal_model    : model -> model -> bool
  val make           : model -> widget * (model -> unit)
  val widget         : widget -> Widget.t

end

module type Array_node = sig

  include Node

  module Id : sig
    type t
    val of_string : string -> t option
    val to_string : t -> string
    val compare : t -> t -> int
  end

  val id_of_model  : model -> Id.t

end

module type Array_root_node = sig

  module Node : Array_node

  type widget

  val make : Node.model list -> widget * (Node.model list -> unit)
  val root : widget -> Dom_html.element Js.t

end

module Make_array(M:Array_root_node) = struct

  type id        = M.Node.Id.t
  type model     = M.Node.model
  type widget    = M.Node.widget

  type nodes =
    { mutable hidden : (widget * (model -> unit)) list
    ; mutable active : (widget * (model -> unit)) list
    }

  let id_of_widget (w:widget) =
    (M.Node.widget w)#get_attribute "data-id"
    |> Option.get_exn
    |> M.Node.Id.of_string
    |> Option.get_exn
  let id_to_widget (id:id) (w:widget) =
    let id = M.Node.Id.to_string id in
    (M.Node.widget w)#set_attribute "data-id" id

  let eq_id (x1:model) (x2:model) =
    match M.Node.Id.compare (M.Node.id_of_model x1) (M.Node.id_of_model x2) with
    | 0 -> true | _ -> false

  let find_opt (id:id) (l:widget list) : widget option =
    List.find_opt (fun x -> 0 = M.Node.Id.compare id @@ id_of_widget x) l

  let insert_before
        ~(parent: #Dom.node Js.t)
        (nodes  : nodes)
        (widget, upd) =
    let c = List.find_opt (fun (x:widget) ->
                match M.Node.Id.compare
                        (id_of_widget widget)
                        (id_of_widget x) with
                | -1 -> true
                | _  -> false) @@ List.map fst nodes.active
            |> Option.map (fun x -> (M.Node.widget x)#root)
            |> Js.Opt.option in
    Dom.insertBefore parent (M.Node.widget widget)#root c;
    nodes.active <- (widget, upd) :: nodes.active

  let partition (o:model list) (n:model list) =
    let lost =
      List.filter (fun x -> not @@ List.mem ~eq:eq_id x n) o in
    let found, changed =
      List.partition_map (fun x ->
          match List.find_opt (eq_id x) o with
          | Some i -> if M.Node.equal_model x i then `Drop else `Right x
          | None   -> `Left x) n in
    lost, found, changed

  let handle_found parent (found:model list) (nodes:nodes) =
    let rec aux acc hidden = function
      | [ ]      -> nodes.hidden <- hidden; acc
      | hd :: tl ->
         (match hidden with
          | [ ]  ->
             let node = M.Node.make hd in
             let ()   = id_to_widget (M.Node.id_of_model hd) (fst node) in
             aux (node :: acc) [] tl
          | (w, upd) :: r ->
             let ()   = id_to_widget (M.Node.id_of_model hd) w in
             upd hd; aux ((w, upd) :: acc) r tl) in
    let found = aux [] nodes.hidden found in
    List.iter (insert_before ~parent nodes) found

  let handle_lost parent (lost:model list) (nodes:nodes) =
    List.iter (fun (x:model) ->
        let id = M.Node.id_of_model x in
        match List.find_idx (fun (w,_) ->
                  0 = M.Node.Id.compare id @@ id_of_widget w)
                nodes.active with
        | Some (idx,node) ->
           let active' = List.remove_at_idx idx nodes.active in
           let () = nodes.active <- active' in
           let () = nodes.hidden <- node :: nodes.hidden in
           (try Dom.removeChild parent
                @@ (M.Node.widget (fst node))#root with _ -> ())
        | None -> ()) lost

  let handle_changed (changed:model list) (nodes:nodes) =
    List.iter (fun (x:model) ->
        let id = M.Node.id_of_model x in
        match List.find_opt (fun (w,_) ->
                  0 = M.Node.Id.compare id @@ id_of_widget w)
                nodes.active with
        | Some (_,upd) -> upd x
        | None         -> ()) changed

  let make (init:model list) =
    let prev   = ref [] in
    let (nodes:nodes) =
      { hidden = []
      ; active = [] } in
    let leaf, update' = M.make init in
    let update = fun model ->
      let lost, found, changed = partition !prev model in
      handle_found (M.root leaf) found nodes;
      handle_lost  (M.root leaf) lost  nodes;
      handle_changed changed nodes;
      update' model;
      prev := model in
    update init;
    leaf, update

end

let ( % ) = Fun.( % )

type item = (unit, unit Tree.t) Tree.Item.t

module General = struct

  type model = general_info

  let make (init:model) =
    let open Printf in
    let open Fun in
    let make_item () = new Tree.Item.t ~text:"" ~value:() () in
    let nw_pid, nw_pid_set =
      let to_string x = sprintf "Network PID: %d" x in
      let w = make_item () in
      let v = { get = (fun x -> x.nw_pid)
              ; eq  = Int.equal
              ; upd = (w#item#set_text % to_string) } in
      w, v in
    let ts_id, ts_id_set =
      let to_string x = sprintf "TS ID: %d" x in
      let w = make_item () in
      let v = { get = (fun x -> x.ts_id)
              ; eq  = Int.equal
              ; upd = (w#item#set_text % to_string) } in
      w, v in
    let nw_id, nw_id_set =
      let to_string x = sprintf "Network ID: %d" x in
      let w = make_item () in
      let v = { get = (fun x -> x.nw_id)
              ; eq  = Int.equal
              ; upd = (w#item#set_text % to_string) } in
      w, v in
    let orig_nw_id, orig_nw_id_set =
      let to_string x = sprintf "Original network ID: %d" x in
      let w = make_item () in
      let v = { get = (fun x -> x.orig_nw_id)
              ; eq  = Int.equal
              ; upd = (w#item#set_text % to_string) } in
      w, v in
    let nw_name, nw_name_set =
      let to_string x = Printf.sprintf "Network name: %s" x in
      let w = make_item () in
      let v = { get = (fun x -> x.nw_name)
              ; eq  = String.equal
              ; upd = (w#item#set_text % to_string) } in
      w, v in
    let bouquet_name, bq_name_set =
      let to_string x = Printf.sprintf "Bouquet name: %s" x in
      let w = make_item () in
      let v = { get = (fun x -> x.bouquet_name)
              ; eq  = String.equal
              ; upd = (w#item#set_text % to_string) } in
      w, v in
    let prev   = ref init in
    let items  = [ nw_pid; ts_id; nw_id; orig_nw_id; nw_name; bouquet_name ] in
    let nested = new Tree.t ~items () in
    let ()     = nested#set_dense true in
    let leaf   = new Tree.Item.t
                   ~text:"Сведения о потоке"
                   ~nested
                   ~value:()
                   () in
    let update = fun ?previous model ->
      let f x = setter ?previous model x in
      f nw_pid_set;
      f ts_id_set;
      f nw_id_set;
      f orig_nw_id_set;
      f nw_name_set;
      f bq_name_set;
      prev := model in
    update init;
    leaf, fun x -> update ~previous:!prev x

end

module Pid = struct

  module Id = Int

  type model  = pid_info [@@deriving eq]
  type widget = item

  let widget         = fun x -> x#widget
  let id_of_model    = fun (x:model) -> x.pid

  let make (init:model) : item * (model -> unit) =
    let to_string = Printf.sprintf "PID: %d" in
    let scrambled = new Icon.SVG.t ~icon:Lock () in
    let pts  = new Icon.SVG.t ~icon:Clock_outline () in
    let meta = new Hbox.t ~widgets:[scrambled; pts] () in
    let prev = ref init in
    let leaf = new Tree.Item.t ~text:"" ~meta:meta#widget ~value:() () in
    let pid_set =
      { get = (fun (x:model) -> x.pid)
      ; eq  = (=)
      ; upd = (fun pid ->
        leaf#item#set_text @@ to_string pid;
        leaf#set_attribute "data-pid" (string_of_int pid)) } in
    let scr_set =
      { get = (fun (x:model) -> x.scrambled)
      ; eq  = Equal.bool
      ; upd = (fun x ->
        let s = if x then "" else "none" in
        scrambled#style##.display := Js.string s) } in
    let pts_set =
      { get = (fun (x:model) -> x.has_pts)
      ; eq  = Equal.bool
      ; upd = (fun x ->
        let s = if x then "" else "none" in
        pts#style##.display := Js.string s) } in
    let update = fun ?previous model ->
      let f x = setter ?previous model x in
      f pid_set; f scr_set; f pts_set;
      prev := model in
    update init;
    leaf, fun x -> update ~previous:!prev x

end

module Pids =
  Make_array(struct
      type widget = item
      module Node = Pid

      let root (w:widget) =
        (Option.get_exn w#nested_tree)#root
      let make _ =
        let nested = new Tree.t ~items:[] () in
        let ()     = nested#set_dense true in
        let leaf   = new Tree.Item.t ~text:"PIDs" ~nested ~value:() () in
        let update = fun model ->
          match model with
          | [ ] -> leaf#style##.display := Js.string "none";
          | l   -> leaf#style##.display := Js.string "" in
        leaf, update
    end)

module ES_pid = struct

  module Id = Int

  type model  = es_info [@@deriving eq]
  type widget = item

  let id_of_model = fun (x:model) -> x.pid
  let widget      = fun w -> w#widget

  let make (init:model) : item * (model -> unit) =
    let to_primary = Printf.sprintf "ES PID: %d" in
    let to_secondary = Printf.sprintf "Тип %d, Stream ID: %d" in
    let pts  = new Icon.SVG.t ~icon:Clock_outline () in
    let prev = ref init in
    let leaf = new Tree.Item.t
                 ~text:""
                 ~secondary_text:""
                 ~meta:pts#widget
                 ~value:() () in
    let pid_set =
      { get = (fun (x:model) -> x.pid)
      ; eq  = (=)
      ; upd = (fun pid ->
        leaf#item#set_text @@ to_primary pid;
        leaf#set_attribute "data-pid" (string_of_int pid)) } in
    let type_set =
      { get = (fun (x:model) -> x.es_type)
      ; eq  = Int.equal
      ; upd = (fun typ ->
        leaf#item#set_secondary_text (to_secondary typ !prev.es_stream_id))
      } in
    let stream_id_set =
      { get = (fun (x:model) -> x.es_stream_id)
      ; eq  = Int.equal
      ; upd = (fun sid ->
        leaf#item#set_secondary_text (to_secondary !prev.es_type sid))
      } in
    let pts_set =
      { get = (fun (x:model) -> x.has_pts)
      ; eq  = Equal.bool
      ; upd = (fun x ->
        let s = if x then "" else "none" in
        pts#style##.display := Js.string s) } in
    let update = fun ?previous model ->
      let f x = setter ?previous model x in
      f pid_set; f pts_set; f type_set; f stream_id_set;
      prev := model in
    update init;
    leaf, fun x -> update ~previous:!prev x

end

module ES_pids =
  Make_array(struct
      type widget = item
      module Node = ES_pid

      let root (w:widget) =
        (Option.get_exn w#nested_tree)#root
      let make _ =
        let nested = new Tree.t ~two_line:true ~items:[] () in
        let ()     = nested#set_dense true in
        let leaf   = new Tree.Item.t ~text:"Элементарные потоки"
                       ~nested ~value:() () in
        let update = fun model ->
          match model with
          | [ ] -> leaf#style##.display := Js.string "none";
          | l   -> leaf#style##.display := Js.string "" in
        leaf, update
    end)

module ECM_pid = struct

  module Id = Int

  type model  = ecm_info [@@deriving eq]
  type widget = item

  let id_of_model = fun (x:model) -> x.pid
  let widget      = fun w -> w#widget

  let make (init:model) : item * (model -> unit) =
    let to_primary = Printf.sprintf "ECM PID: %d" in
    let to_secondary = Printf.sprintf "CA System ID: %d" in
    let prev = ref init in
    let leaf = new Tree.Item.t
                 ~text:""
                 ~secondary_text:""
                 ~value:() () in
    let pid_set =
      { get = (fun (x:model) -> x.pid)
      ; eq  = (=)
      ; upd = (fun pid ->
        leaf#item#set_text @@ to_primary pid;
        leaf#set_attribute "data-pid" (string_of_int pid)) } in
    let system_id_set =
      { get = (fun (x:model) -> x.ca_sys_id)
      ; eq  = Int.equal
      ; upd = (fun sid ->
        leaf#item#set_secondary_text (to_secondary sid))
      } in
    let update = fun ?previous model ->
      let f x = setter ?previous model x in
      f pid_set; f system_id_set;
      prev := model in
    update init;
    leaf, fun x -> update ~previous:!prev x

end

module ECM_pids =
  Make_array(struct
      type widget = item
      module Node = ECM_pid

      let root (w:widget) =
        (Option.get_exn w#nested_tree)#root
      let make _ =
        let nested = new Tree.t ~two_line:true ~items:[] () in
        let ()     = nested#set_dense true in
        let leaf   = new Tree.Item.t ~text:"ECM"
                       ~nested ~value:() () in
        let update = fun model ->
          match model with
          | [ ] -> leaf#style##.display := Js.string "none";
          | l   -> leaf#style##.display := Js.string "" in
        leaf, update
    end)

module Service = struct

  module Id = Int

  type model  = service_info [@@deriving eq]
  type widget = item

  let id_of_model = fun (x:model) -> x.id
  let widget = fun w -> w#widget

  let make (init:model) =
    let open Printf in
    let make_item () = new Tree.Item.t ~text:"" ~value:() () in
    let id, update_id =
      let to_string x = sprintf "ID: %d" x in
      let w = make_item () in
      let v = { get = (fun (x:model) -> x.id)
              ; eq  = Int.equal
              ; upd = (w#item#set_text % to_string) } in
      w, v in
    let pmt, update_pmt =
      let to_string x = sprintf "PMT PID: %d" x in
      let w = make_item () in
      let v = { get = (fun (x:model) -> x.pmt_pid)
              ; eq  = Int.equal
              ; upd = (w#item#set_text % to_string) } in
      w, v in
    let pcr, update_pcr =
      let to_string x = sprintf "PCR PID: %d" x in
      let w = make_item () in
      let v = { get = (fun (x:model) -> x.pcr_pid)
              ; eq  = Int.equal
              ; upd = (w#item#set_text % to_string) } in
      w, v in
    let es, update_es   = ES_pids.make init.es in
    let ecm, update_ecm = ECM_pids.make init.ecm in
    let opt     = es :: ecm :: [] in
    let nested  = new Tree.t ~items:([ id; pmt; pcr ] @ opt) () in
    let ()      = nested#set_dense true in
    let graphic = new Icon.SVG.t ~icon:Tv () in
    let prev    = ref init in
    let leaf, update_name, update_prov =
      let to_primary x = x in
      let to_secondary x = sprintf "Провайдер: %s" x in
      let w = new Tree.Item.t
                ~text:""
                ~secondary_text:""
                ~graphic
                ~nested
                ~value:() () in
      let () = w#set_attribute "data-id" @@ string_of_int init.id in
      let v_name =
        { get = (fun (x:model) -> x.name)
        ; eq  = String.equal
        ; upd = (w#item#set_text % to_primary) } in
      let v_prov =
        { get = (fun (x:model) -> x.provider_name)
        ; eq  = String.equal
        ; upd = (w#item#set_secondary_text % to_secondary) } in
      w, v_name, v_prov in
    let update = fun ?previous (model:model) ->
      setter ?previous model update_id;
      setter ?previous model update_pmt;
      setter ?previous model update_pcr;
      setter ?previous model update_name;
      setter ?previous model update_prov;
      update_es model.es;
      update_ecm model.ecm in
    update init;
    leaf, fun x -> update ~previous:!prev x

end

module Services =
  Make_array(struct
      type widget = item
      module Node = Service

      let root (w:widget) =
        (Option.get_exn w#nested_tree)#root
      let make _ =
        let nested = new Tree.t ~two_line:true ~items:[] () in
        let ()     = nested#set_dense true in
        let leaf   = new Tree.Item.t ~text:"Сервисы"
                       ~nested ~value:() () in
        let update = fun model ->
          match model with
          | [ ] -> leaf#style##.display := Js.string "none";
          | l   -> leaf#style##.display := Js.string "" in
        leaf, update
    end)

module EMM_pid = struct

  module Id = Int

  type model  = emm_info [@@deriving eq]
  type widget = item

  let id_of_model = fun (x:model) -> x.pid
  let widget      = fun w -> w#widget

  let make (init:model) : item * (model -> unit) =
    let to_primary = Printf.sprintf "EMM PID: %d" in
    let to_secondary = Printf.sprintf "CA System ID: %d" in
    let prev = ref init in
    let leaf = new Tree.Item.t
                 ~text:""
                 ~secondary_text:""
                 ~value:() () in
    let pid_set =
      { get = (fun (x:model) -> x.pid)
      ; eq  = (=)
      ; upd = (fun pid ->
        leaf#item#set_text @@ to_primary pid;
        leaf#set_attribute "data-pid" (string_of_int pid)) } in
    let system_id_set =
      { get = (fun (x:model) -> x.ca_sys_id)
      ; eq  = Int.equal
      ; upd = (fun sid ->
        leaf#item#set_secondary_text (to_secondary sid))
      } in
    let update = fun ?previous model ->
      let f x = setter ?previous model x in
      f pid_set; f system_id_set;
      prev := model in
    update init;
    leaf, fun x -> update ~previous:!prev x

end

module EMM_pids =
  Make_array(struct
      type widget = item
      module Node = EMM_pid

      let root (w:widget) =
        (Option.get_exn w#nested_tree)#root
      let make _ =
        let nested = new Tree.t ~two_line:true ~items:[] () in
        let ()     = nested#set_dense true in
        let leaf   = new Tree.Item.t ~text:"EMM"
                       ~nested ~value:() () in
        let update = fun model ->
          match model with
          | [ ] -> leaf#style##.display := Js.string "none";
          | l   -> leaf#style##.display := Js.string "" in
        leaf, update
    end)

type (_,_) sdom =
  | Node  : ('a * (module Array_node)) * ('b, 'c) sdom -> ('a -> 'b, 'c) sdom
  | Array : ('a list * (module Array_node)) * ('b, 'c) sdom -> ('a -> 'b, 'c) sdom
  | Empty : ('c, 'c) sdom

let sdom = Array ((List.empty, (module Pid)), Empty)

module Section = struct

  module Id = Int

  type model  =
    { stream  : Stream.id
    ; table   : table
    ; push    : dumpable -> unit
    ; control : int
    ; section : section_info
    }
  type widget = item

  let equal_model = (fun x1 x2 -> equal_section_info x1.section x2.section)
  let id_of_model = fun (x:model)  -> x.section.id
  let widget      = fun w -> w#widget

  let req_of_table (model:model) =
    let control = model.control in
    let common  = table_common_of_table model.table in
    let r = Requests.Stream.HTTP.TS.get_si_psi_section
              ~id:model.stream
              ~table_id:common.id
              ~section:model.section.id in
    match model.table with
    | PAT x -> r ~table_id_ext:x.ts_id control
    | PMT x -> r ~table_id_ext:x.program_number control
    | NIT x -> r ~table_id_ext:x.nw_id control
    | SDT x -> r ~table_id_ext:x.ts_id control
    | BAT x -> r ~table_id_ext:x.bouquet_id control
    | EIT x -> r ~table_id_ext:x.service_id
                 ~eit_ts_id:x.params.ts_id
                 ~eit_orig_nw_id:x.params.orig_nw_id
                 control
    | _     -> r control

  let to_section_name table section =
    let divider = ", " in
    let common  = table_common_of_table table in
    let name    = table_to_string table in
    let id s x  = Printf.sprintf "%s=0x%02X(%d)" s x x in
    let base    = id "table_id" common.id in
    let section = Printf.sprintf "секция %d" section in
    let specific = match table with
      | PAT x -> Some [ id "ts_id" x.ts_id ]
      | PMT x -> Some [ id "program_number" x.program_number ]
      | NIT x -> Some [ id "network_id" x.nw_id ]
      | SDT x -> Some [ id "ts_id" x.ts_id ]
      | BAT x -> Some [ id "bouquet_id" x.bouquet_id ]
      | EIT x -> Some [ id "service_id" x.service_id
                      ; id "ts_id" x.params.ts_id
                      ; id "original_network_id" x.params.orig_nw_id ]
      | _     -> None in
    match specific with
    | Some l -> name, String.concat divider (base :: l @ [ section ])
    | None   -> name, base ^ divider ^ section

  let make (init:model) =
    let open Lwt_result.Infix in
    let s_dump, s_dump_push  = React.S.create None in
    let base_class = Markup.CSS.add_element base_class "section-item" in
    let prev    = ref init in
    let graphic = new Icon.SVG.t ~icon:Download () in
    let bytes, update_bytes =
      let to_string x =
        let s = if x > 1 && x < 5 then "байта" else "байт" in
        Printf.sprintf "%d %s" x s in
      let w = new Typography.Text.t ~text:"" () in
      let v = { get = (fun (x:model) -> x.section.length)
              ; eq  = Int.equal
              ; upd = (w#set_text % to_string) } in
      w, v in
    let leaf = new Tree.Item.t
                 ~text:""
                 ~graphic
                 ~meta:bytes#widget
                 ~value:() () in
    let update_id =
      let to_string x = Printf.sprintf "ID: %d" x in
      { get = (fun (x:model) -> x.section.id)
      ; eq  = Int.equal
      ; upd = (leaf#item#set_text % to_string) } in
    Dom_events.listen leaf#item#root Dom_events.Typ.click (fun _ _ ->
        let get = fun () ->
          req_of_table !prev
          >|= (fun dump -> s_dump_push (Some dump.section); dump.section)
          |> Lwt_result.map_err Api_js.Requests.err_to_string in
        let name     = to_section_name init.table !prev.section.id in
        let dumpable = { name; get; prev = s_dump } in
        init.push dumpable; true) |> ignore;
    let () = leaf#item#add_class base_class in
    let update = fun ?previous (model:model) ->
      setter ?previous model update_id;
      setter ?previous model update_bytes;
      prev := model in
    update init;
    leaf, fun x -> update ~previous:!prev x

end

module Sections =
  Make_array(struct
      type widget = item
      module Node = Section

      let root (w:widget) =
        (Option.get_exn w#nested_tree)#root
      let make _ =
        let nested = new Tree.t ~two_line:false ~items:[] () in
        let ()     = nested#set_dense true in
        let leaf   = new Tree.Item.t ~text:"Секции"
                       ~nested ~value:() () in
        let update = fun model ->
          match model with
          | [ ] -> leaf#style##.display := Js.string "none";
          | l   -> leaf#style##.display := Js.string "" in
        leaf, update
    end)

module Table = struct

  module Id = struct
    type t     =
      { id        : int
      ; id_ext    : int
      ; eit_ts_id : int
      ; eit_nw_id : int
      } [@@deriving yojson]
    let compare x1 x2 = compare x1.id x2.id
    let to_string x = to_yojson x |> Yojson.Safe.to_string
    let of_string s = Yojson.Safe.from_string s
                      |> of_yojson
                      |> Result.to_opt
  end

  type model =
    { stream  : Stream.id
    ; push    : dumpable -> unit
    ; control : int
    ; table   : table
    }
  type widget = item

  let widget      = fun w -> w#widget
  let equal_model = fun x1 x2 -> equal_table x1.table x2.table
  let id_of_model = fun (x:model)  ->
    let open Id in
    let c   = table_common_of_table x.table in
    let def = { id = c.id; id_ext = 0; eit_ts_id = 0; eit_nw_id = 0 } in
    match x.table with
    | PAT x -> { def with id_ext = x.ts_id }
    | PMT x -> { def with id_ext = x.program_number }
    | NIT x -> { def with id_ext = x.nw_id }
    | SDT x -> { def with id_ext = x.ts_id }
    | BAT x -> { def with id_ext = x.bouquet_id }
    | EIT x -> { def with id_ext = x.service_id
                        ; eit_ts_id = x.params.ts_id
                        ; eit_nw_id = x.params.orig_nw_id }
    | _     -> def

  let make (init:model) =
    let to_primary   = Printf.sprintf "%s, PID: %d"  in
    let to_secondary = Printf.sprintf "Версия: %d, ID: %d, LSN: %d" in
    (* let make_item = new Tree.Item.t ~value:() in *)
    (* let specific = match table with
     *   | PAT x ->
     *      [ make_item ~text:(Printf.sprintf "TS ID: %d" x.ts_id) () ]
     *   | PMT x ->
     *      [ make_item ~text:(Printf.sprintf "Номер программы: %d" x.program_number) () ]
     *   | NIT x ->
     *      [ make_item ~text:(Printf.sprintf "Network ID: %d" x.nw_id) () ]
     *   | SDT x ->
     *      [ make_item ~text:(Printf.sprintf "TS ID: %d" x.ts_id) () ]
     *   | BAT x ->
     *      [ make_item ~text:(Printf.sprintf "Bouquet ID: %d" x.bouquet_id) () ]
     *   | EIT x ->
     *      [ make_item ~text:(Printf.sprintf "Service ID: %d" x.service_id) ()
     *      ; make_item ~text:(Printf.sprintf "TS ID: %d" x.params.ts_id) ()
     *      ; make_item ~text:(Printf.sprintf "Oririnal network ID: %d" x.params.orig_nw_id) ()
     *      ; make_item ~text:(Printf.sprintf "Segment LSN: %d" x.params.segment_lsn) ()
     *      ; make_item ~text:(Printf.sprintf "Last table ID: %d" x.params.last_table_id) () ]
     *   | _ -> []
     * in *)
    let map = fun stream table control push sections ->
      let open Section in
      List.map (fun x -> { stream; table; control; push; section = x }) sections in
    let sections, update_sections =
      Sections.make (map init.stream
                       init.table
                       init.control
                       init.push
                       (table_common_of_table init.table).sections) in
    let nested = new Tree.t ~items:((* specific @ *) [sections]) () in
    let ()     = nested#set_dense true in
    let prev   = ref init in
    let leaf   = new Tree.Item.t
                   ~text:""
                   ~secondary_text:""
                   ~nested
                   ~value:() () in
    let update_primary =
      { get = (fun x -> let c = table_common_of_table x.table in
                        c.pid, table_to_string x.table)
      ; eq  = (fun (x1,y1) (x2,y2) ->
        x1 = x2 && String.equal y1 y2)
      ; upd = (fun (pid,s) ->
        let s = to_primary s pid in
        leaf#item#set_text s)
      } in
    let update_secondary =
      { get = (fun x -> let c = table_common_of_table x.table in
                        c.version, c.id, c.lsn)
      ; eq  = (fun (x1,y1,z1) (x2,y2,z2) ->
        x1 = x2 && y1 = y2 && z1 = z2)
      ; upd = (fun (v,id,lsn) ->
        let s = to_secondary v id lsn in
        leaf#item#set_secondary_text s)
      } in
    let update = fun ?(previous:model option) (model:model) ->
      let common = table_common_of_table model.table in
      setter ?previous model update_primary;
      setter ?previous model update_secondary;
      update_sections @@ map model.stream
                           model.table
                           model.control
                           model.push
                           common.sections; in
    leaf, fun x -> update ~previous:!prev x

end

module Tables =
  Make_array(struct
      type widget = item
      module Node = Table

      let root (w:widget) =
        (Option.get_exn w#nested_tree)#root
      let make _ =
        let nested = new Tree.t ~two_line:true ~items:[] () in
        let ()     = nested#set_dense true in
        let leaf   = new Tree.Item.t ~text:"Таблицы"
                       ~nested ~value:() () in
        let update = fun model ->
          match model with
          | [ ] -> leaf#style##.display := Js.string "none";
          | l   -> leaf#style##.display := Js.string "" in
        leaf, update
    end)

let make_stream (id:Stream.id)
      (init:  structure)
      (event: structure React.event)
      (push:  dumpable -> unit)
      control =
  let map_tables =
    let open Table in
    List.map (fun x -> { stream = id; push; control; table = x }) in
  let event = React.S.diff (fun n o -> o, n)
              @@ React.S.hold ~eq:(equal_structure) init event in
  let gen,  update_gen  = General.make init.general in
  let pids, update_pids = Pids.make init.pids in
  let serv, update_serv = Services.make init.services in
  let emm,  update_emm  = EMM_pids.make init.emm in
  let tabl, update_tabl = Tables.make (map_tables init.tables) in
  let time, update_time =
    let to_string x =
      let (y,m,d),((h,min,s),_) =
        Time.to_date_time
          ?tz_offset_s:(Ptime_clock.current_tz_offset_s ()) x in
      Printf.sprintf "Получена: %02d.%02d.%04d %02d:%02d:%02d" d m y h min s in
    let w = new Tree.Item.t
              ~text:""
              ~value:() () in
    let upd = fun (model:Time.t) -> w#item#set_text @@ to_string model in
    w, upd in
  let _ =
    React.E.map (fun (prev,model) ->
        if not @@ equal_general_info prev.general model.general
        then (print_endline "general changed!"; update_gen model.general);
        if not @@ (Equal.list equal_pid_info) prev.pids model.pids
        then (print_endline "pids changed!"; update_pids model.pids);
        if not @@ (Equal.list equal_service_info) prev.services model.services
        then (print_endline "services changed!"; update_serv model.services);
        if not @@ (Equal.list equal_emm_info) prev.emm model.emm
        then (print_endline "emm changed!"; update_emm model.emm);
        if not @@ (Equal.list equal_table) prev.tables model.tables
        then (print_endline "tables changed!";
              update_tabl @@ map_tables model.tables);
        if not @@ Time.equal prev.timestamp model.timestamp
        then (print_endline "timestamp changed!"; update_time model.timestamp))
      event in
  let opt  = (serv :: tabl :: pids :: emm :: []) in
  let tree = new Tree.t ~items:(time :: gen :: opt) () in
  let ()   = tree#set_dense true in
  tree

let make_parsed () =
  let base_class = Markup.CSS.add_element base_class "parsed" in
  let body       = Dom_html.createDiv Dom_html.document |> Widget.create in
  let ()         = body#add_class base_class in
  body#widget

let make_hexdump_options hexdump =
  let base_class = Markup.CSS.add_element base_class "hexdump-options" in
  let base =
    new Select.t
      ~label:"Основание"
      ~items:[ `Item (new Select.Item.t ~value:`Hex ~text:"16" ())
             ; `Item (new Select.Item.t ~value:`Dec ~text:"10" ())
             ; `Item (new Select.Item.t ~value:`Bin ~text:"2" ()) ]
      () in
  let width =
    new Select.t
      ~label:"Ширина"
      ~items:[ `Item (new Select.Item.t ~value:4  ~text:"4"  ())
             ; `Item (new Select.Item.t ~value:8  ~text:"8" ~selected:true ())
             ; `Item (new Select.Item.t ~value:16 ~text:"16" ())
             ; `Item (new Select.Item.t ~value:32 ~text:"32" ()) ]
      () in
  let line_numbers  = new Switch.t ~state:true () in
  let line_numbers' = new Form_field.t ~input:line_numbers
                        ~label:"Номера" () in
  let options = new Hbox.t
                  ~widgets:[ base#widget
                           ; width#widget
                           ; line_numbers'#widget ] () in
  let () = options#add_class base_class in
  let _  = React.S.map hexdump#set_line_numbers line_numbers#s_state in
  let _  = React.S.map (function
               | Some x -> hexdump#set_width x
               | None   -> ()) width#s_selected_value in
  let _  = React.S.map (function
               | Some x -> hexdump#set_base x
               | None   -> ()) base#s_selected_value in
  options#widget

let make_hexdump (signal:string React.signal) =
  let config  = Hexdump.to_config ~width:16 () in
  let hexdump = new Hexdump.t ~config "" () in
  let _s = React.S.map hexdump#set_bytes signal in
  Lwt_react.S.keep _s;
  hexdump

let make_dump_header base_class () =
  (* CSS classes *)
  let header_class = Markup.CSS.add_element base_class "header" in
  let title_class  = Markup.CSS.add_element base_class "title" in
  (* Elements *)
  let title     = new Typography.Text.t
                    ~adjust_margin:false
                    ~text:"Выберите секцию таблицы SI/PSI для захвата" () in
  let subtitle  = new Typography.Text.t
                    ~adjust_margin:false
                    ~split:true
                    ~text:"" () in
  let button    = new Ui_templates.Buttons.Get.t
                    ~style:`Raised
                    ~label:"Загрузить" () in
  let title_box = new Vbox.t
                    ~widgets:[ title#widget
                             ; subtitle#widget ] () in
  let header    = new Hbox.t
                    ~halign:`Space_between
                    ~widgets:[ title_box#widget
                             ; button#widget] () in
  (* CSS classes setup *)
  let () = title#add_class title_class in
  let () = header#add_class header_class in
  header#widget, title, subtitle, button

let make_dump (event:dumpable React.event) =
  let base_class = Markup.CSS.add_element base_class "dump" in
  let header, title, subtitle, button = make_dump_header base_class () in
  let s, push = React.S.create "" in
  let parsed  = make_parsed () in
  let hexdump = make_hexdump s in
  let options = make_hexdump_options hexdump in
  let () =
    React.E.map (fun {name;get;prev} ->
        let open Lwt.Infix in
        let text   = new Typography.Text.t ~text:"" () in
        let err x  = Ui_templates.Placeholder.create_with_error ~text:x () in
        let ph  x  = Ui_templates.Placeholder.create_with_icon
                       ~icon:"info"
                       ~text:x () in
        let get    = fun () ->
          Lwt.catch (fun () ->
              get ()
              >|= (function
                   | Ok _    -> parsed#set_empty ();
                                Dom.appendChild parsed#root text#root
                   | Error s -> parsed#set_empty ();
                                Dom.appendChild parsed#root (err s)#root))
            (fun e ->
              parsed#set_empty ();
              Dom.appendChild parsed#root (err @@ Printexc.to_string e)#root;
              Lwt.return_unit) in
        let _s   =
          React.S.map (function
              | Some raw ->
                 parsed#set_empty ();
                 text#set_text raw;
                 Dom.appendChild parsed#root text#root;
                 push raw
              | None     ->
                 parsed#set_empty ();
                 Dom.appendChild parsed#root (ph "Нет захваченных данных")#root;
                 push "") prev in
        let () = button#set_getter (Some get) in
        let () = title#set_text @@ fst name in
        let () = subtitle#set_text @@ snd name in
        let () = button#set_disabled false in
        _s) event
    |> Lwt_react.E.keep in
  let vsplit = new Vsplit.t parsed hexdump () in
  let vbox   = new Vbox.t ~widgets:[ header
                                   ; (new Divider.t ())#widget
                                   ; vsplit#widget
                                   ; (new Divider.t ())#widget
                                   ; options#widget ] () in
  vbox#add_class base_class;
  vbox#widget

class t ?(config=default_config)
        ~(state:Common.Topology.state React.signal)
        ~(stream:Common.Stream.t option React.signal)
        ~(init:structure option)
        ~(event:structure option React.event)
        (control:int)
        () =
  let ( >|= ) = Lwt.Infix.( >|= ) in
  let stream_panel_class = Markup.CSS.add_element base_class "stream-panel" in
  let id  = config.stream in
  let ph  = Ui_templates.Placeholder.create_with_icon
              ~icon:"warning" ~text:"Нет потока" () in
  let box = Dom_html.createDiv Dom_html.document
            |> Widget.create in
  let e_dumpable, push = React.E.create () in
  let ()  = Dom.appendChild box#root ph#root in
  let make_event init = React.E.fmap (fun x -> x) event in
  let make_struct init =
    let event = make_event init in
    let wdg   = make_stream id init event push control in
    wdg in
  let ts_s = match init with
    | None ->
       let next = Lwt_react.E.next @@ React.E.fmap (fun x -> x) event in
       next >|= (make_struct)
    | Some init ->
       let wdg   = make_struct init in
       Lwt.return wdg in
  let dump = make_dump e_dumpable in
  object(self)
    inherit Hsplit.t box dump ()

    initializer
      Dom.appendChild box#root ph#root;
      ts_s
      >|= (fun w -> box#set_empty (); Dom.appendChild box#root w#root)
      |> Lwt.ignore_result;
      box#add_class stream_panel_class;
      self#add_class base_class
  end

let make
      ?(config:config option)
      ~(state:Common.Topology.state React.signal)
      ~(stream:Common.Stream.t option React.signal)
      ~(init:structure option)
      ~(event:structure option React.event)
      (control:int)
      () =
  new t ?config ~state ~stream ~init ~event control ()
