open Containers
open Components
open Board_types.Streams.TS
open Common
open Widget_structure_common

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

  val equal_model : model -> model -> bool
  val make        : model -> widget * (model -> unit)
  val widget      : widget -> Widget.t

end

module type Array_node = sig

  include Node

  module Id : sig
    type t
    val of_string : string -> t option
    val to_string : t -> string
    val compare   : t -> t -> int
  end

  val id_of_model : model -> Id.t

end

module type Array_root_node = sig

  module Node : Array_node

  type widget

  val make : Node.model list -> widget * (Node.model list -> unit)
  val root : widget -> Dom_html.element Js.t

end

module Make_array(M:Array_root_node) = struct

  type id     = M.Node.Id.t
  type model  = M.Node.model
  type widget = M.Node.widget

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
    let c = List.find_idx (fun (x:widget) ->
                match M.Node.Id.compare
                        (id_of_widget x)
                        (id_of_widget widget) with
                | 1 -> true
                | _ -> false) @@ List.map fst nodes.active
            |> Option.map (fun (id, x) -> id, (M.Node.widget x)#root) in
    match c with
    | Some (id, x) ->
       Dom.insertBefore parent (M.Node.widget widget)#root (Js.some x);
       nodes.active <- List.insert_at_idx id (widget, upd) nodes.active
    | None ->
       Dom.appendChild parent (M.Node.widget widget)#root;
       nodes.active <- nodes.active @ [ widget, upd ]

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
    let nested = new Tree.t ~level:1 ~items () in
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
    let pcr  = new Icon.SVG.t ~icon:Clock_outline () in
    let meta = new Hbox.t ~widgets:[scrambled; pcr] () in
    let prev = ref init in
    let leaf = new Tree.Item.t ~text:"" ~meta:meta#widget ~value:() () in
    let pid_set =
      { get = (fun (x:model) -> x.pid)
      ; eq  = (=)
      ; upd = (fun pid ->
        leaf#item#set_text @@ to_string pid) } in
    let scr_set =
      { get = (fun (x:model) -> x.scrambled)
      ; eq  = Equal.bool
      ; upd = (fun x ->
        let s = if x then "" else "none" in
        scrambled#style##.display := Js.string s) } in
    let pcr_set =
      { get = (fun (x:model) -> x.has_pcr)
      ; eq  = Equal.bool
      ; upd = (fun x ->
        let s = if x then "" else "none" in
        pcr#style##.display := Js.string s) } in
    let update = fun ?previous model ->
      let f x = setter ?previous model x in
      f pid_set; f scr_set; f pcr_set;
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
        let nested = new Tree.t ~level:1 ~items:[] () in
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
    let to_primary = Printf.sprintf "PID: %d, Stream ID: %d" in
    let to_secondary x =
      let s = Mpeg_ts.stream_type_to_string x in
      Printf.sprintf "%s (%d)" s x in
    let pcr  = new Icon.SVG.t ~icon:Clock_outline () in
    let prev = ref init in
    let leaf = new Tree.Item.t
                 ~text:""
                 ~secondary_text:""
                 ~meta:pcr#widget
                 ~value:() () in
    let primary_set =
      { get = (fun (x:model) -> x.pid, x.es_stream_id)
      ; eq  = Equal.pair (=) (=)
      ; upd = (fun (pid, sid) ->
        leaf#item#set_text @@ to_primary pid sid) } in
    let type_set =
      { get = (fun (x:model) -> x.es_type)
      ; eq  = Int.equal
      ; upd = (fun typ ->
        leaf#item#set_secondary_text (to_secondary typ))
      } in
    let pcr_set =
      { get = (fun (x:model) -> x.has_pcr)
      ; eq  = Equal.bool
      ; upd = (fun x ->
        let s = if x then "" else "none" in
        pcr#style##.display := Js.string s) } in
    let update = fun ?previous model ->
      let f x = setter ?previous model x in
      f primary_set; f pcr_set; f type_set;
      prev := model in
    update init;
    leaf, fun x -> update ~previous:!prev x

end

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
        leaf#item#set_text @@ to_primary pid) } in
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
        let nested = new Tree.t ~level:3 ~two_line:true ~items:[] () in
        let ()     = nested#set_dense true in
        let leaf   = new Tree.Item.t ~text:"ECM"
                       ~nested ~value:() () in
        let update = fun model ->
          match model with
          | [ ] -> leaf#style##.display := Js.string "none";
                   leaf#collapse ();
          | l   -> leaf#style##.display := Js.string "" in
        leaf, update
    end)

module ES_pids =
  Make_array(struct
      type widget = unit Tree.t
      module Node = ES_pid

      let root (w:widget) = w#root
      let make _ =
        let nested = new Tree.t ~level:2 ~two_line:true ~items:[] () in
        let ()     = nested#set_dense true in
        let update = fun model ->
          match model with
          | [ ] -> nested#style##.display := Js.string "none";
          | l   -> nested#style##.display := Js.string "" in
        nested, update
    end)

module Service = struct

  module Id = Int

  type model  = service_info [@@deriving eq]
  type widget = item

  let id_of_model = fun (x:model) -> x.id
  let widget = fun w -> w#widget

  let make (init:model) =
    let open Printf in
    let es,  update_es  = ES_pids.make init.es in
    let ecm, update_ecm = ECM_pids.make init.ecm in
    let ()      = Dom.appendChild es#root ecm#root in
    let graphic = new Icon.SVG.t ~icon:Tv () in
    let prev    = ref init in
    let leaf, update_primary, update_secondary =
      let to_primary x = x in
      let to_secondary x = sprintf "ID: %d, Провайдер: %s" x in
      let w = new Tree.Item.t
                ~text:""
                ~secondary_text:""
                ~graphic
                ~nested:es
                ~value:() () in
      let v_primary =
        { get = (fun (x:model) -> x.name)
        ; eq  = String.equal
        ; upd = (w#item#set_text % to_primary) } in
      let v_prov =
        { get = (fun (x:model) -> x.id, x.provider_name)
        ; eq  = Equal.pair (=) String.equal
        ; upd = (fun (id, name) ->
          let s = to_secondary id name in
          w#item#set_secondary_text s) } in
      w, v_primary, v_prov in
    let update = fun ?previous (model:model) ->
      setter ?previous model update_primary;
      setter ?previous model update_secondary;
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
        let nested = new Tree.t ~level:1 ~two_line:true ~items:[] () in
        let ()     = nested#set_dense true in
        let leaf   = new Tree.Item.t ~text:"Сервисы"
                       ~nested ~value:() () in
        let update = fun model ->
          match model with
          | [ ] -> leaf#style##.display := Js.string "none";
                   leaf#collapse ()
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
        leaf#item#set_text @@ to_primary pid) } in
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
                   leaf#collapse ();
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
    ; table   : table_info
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
    let table_id_ext = model.table.id_ext in
    let eit_params = model.table.eit_params in
    let r = Requests.Stream.HTTP.TS.get_si_psi_section
              ~id:model.stream
              ~table_id:model.table.id
              ~section:model.section.id in
    match table_of_int model.table.id with
    | `PAT   -> r ~table_id_ext control
    | `PMT   -> r ~table_id_ext control
    | `NIT _ -> r ~table_id_ext control
    | `SDT _ -> r ~table_id_ext control
    | `BAT   -> r ~table_id_ext control
    | `EIT _ -> r ~table_id_ext
                  ~eit_ts_id:eit_params.ts_id
                  ~eit_orig_nw_id:eit_params.orig_nw_id
                  control
    | _      -> r control

  let to_section_name table section =
    let divider  = ", " in
    let name     = table_to_string @@ table_of_int table.id in
    let id s x   = Printf.sprintf "%s=0x%02X(%d)" s x x in
    let base     = id "table_id" table.id in
    let section  = Printf.sprintf "секция %d" section in
    let specific = match table_of_int table.id with
      | `PAT   -> Some [ id "ts_id" table.id_ext ]
      | `PMT   -> Some [ id "program_number" table.id_ext ]
      | `NIT _ -> Some [ id "network_id" table.id_ext ]
      | `SDT _ -> Some [ id "ts_id" table.id_ext ]
      | `BAT   -> Some [ id "bouquet_id" table.id_ext ]
      | `EIT _ -> Some [ id "service_id" table.id_ext
                     ; id "ts_id" table.eit_params.ts_id
                     ; id "original_network_id" table.eit_params.orig_nw_id ]
      | _      -> None in
    match specific with
    | Some l -> name, String.concat divider (base :: l @ [ section ])
    | None   -> name, base ^ divider ^ section

  let make (init:model) =
    let open Lwt_result.Infix in
    let base_class = Markup.CSS.add_element base_class "section-item" in
    let prev' = ref init in
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
      ; upd = (fun x -> leaf#item#set_text @@ to_string x) } in
    Dom_events.listen leaf#item#root Dom_events.Typ.click (fun _ _ ->
        let name = to_section_name init.table !prev'.section.id in
        let prev = ref None in
        let get  = fun () ->
          req_of_table !prev'
          >|= (fun dump -> prev := Some dump; dump)
          |> Lwt_result.map_err Api_js.Requests.err_to_string in
        let dumpable = { name; get; prev } in
        init.push dumpable; true) |> ignore;
    let () = leaf#item#add_class base_class in
    let update = fun ?previous (model:model) ->
      setter ?previous model update_id;
      setter ?previous model update_bytes;
      prev' := model in
    update init;
    leaf, fun x -> update ~previous:!prev' x

end

module Sections =
  Make_array(struct
      type widget = item
      module Node = Section

      let root (w:widget) =
        (Option.get_exn w#nested_tree)#root
      let make _ =
        let nested = new Tree.t ~level:3 ~two_line:false ~items:[] () in
        let ()     = nested#set_dense true in
        let leaf   = new Tree.Item.t ~text:""
                       ~nested ~value:() () in
        let update = fun model ->
          let s = Printf.sprintf "Секции(%d)" @@ List.length model in
          leaf#item#set_text s;
          match model with
          | [ ] -> leaf#style##.display := Js.string "none";
                   leaf#collapse ();
          | l   -> leaf#style##.display := Js.string ""
        in
        leaf, update
    end)

module Table = struct

  module Id = struct
    type t     =
      { id        : int
      ; id_ext    : int
      ; version   : int
      ; eit_ts_id : int
      ; eit_nw_id : int
      } [@@deriving yojson, ord]
    let to_string x   = to_yojson x |> Yojson.Safe.to_string
    let of_string s = Yojson.Safe.from_string s
                      |> of_yojson
                      |> Result.to_opt
  end

  type model =
    { stream  : Stream.id
    ; push    : dumpable -> unit
    ; control : int
    ; table   : table_info
    }
  type widget = item

  let widget      = fun w -> w#widget
  let equal_model = fun x1 x2 -> equal_table_info x1.table x2.table
  let id_of_model = fun (x:model)  ->
    let open Id in
    { id        = x.table.id
    ; version   = x.table.version
    ; id_ext    = x.table.id_ext
    ; eit_ts_id = x.table.eit_params.ts_id
    ; eit_nw_id = x.table.eit_params.orig_nw_id }

  let make (init:model) =
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
                       init.table.sections) in
    let nested = new Tree.t ~level:2 ~items:((* specific @ *) [sections]) () in
    let ()     = nested#set_dense true in
    let prev   = ref init in
    let leaf   = new Tree.Item.t
                   ~text:""
                   ~secondary_text:""
                   ~nested
                   ~value:() () in
    let to_primary   = Printf.sprintf "%s" in
    let to_secondary = Printf.sprintf "PID: %d, версия: %d, ID: %d, LSN: %d" in
    let update_primary =
      { get = (fun x -> table_to_string @@ table_of_int x.table.id)
      ; eq  = String.equal
      ; upd = (fun s ->
        let s = to_primary s in
        leaf#item#set_text s)
      } in
    let update_secondary =
      { get = (fun x -> x.table.pid, x.table.version,
                        x.table.id, x.table.lsn)
      ; eq  = (fun (pid1, ver1, id1, lsn1)
                   (pid2, ver2, id2, lsn2) ->
        pid1 = pid2 && ver1 = ver2 && id1 = id2 && lsn1 = lsn2)
      ; upd = (fun (pid, ver, id, lsn) ->
        let s = to_secondary pid ver id lsn in
        leaf#item#set_secondary_text s)
      } in
    let update = fun ?(previous:model option) (model:model) ->
      setter ?previous model update_primary;
      setter ?previous model update_secondary;
      update_sections @@ map model.stream
                           model.table
                           model.control
                           model.push
                           model.table.sections; in
    update init;
    leaf, fun x -> update ~previous:!prev x

end

module Tables =
  Make_array(struct
      type widget = item
      module Node = Table

      let root (w:widget) =
        (Option.get_exn w#nested_tree)#root
      let make _ =
        let nested = new Tree.t ~level:1 ~two_line:true ~items:[] () in
        let ()     = nested#set_dense true in
        let leaf   = new Tree.Item.t ~text:"Таблицы"
                       ~nested ~value:() () in
        let update = fun model ->
          match model with
          | [ ] -> leaf#style##.display := Js.string "none";
                   leaf#collapse ()
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
      let tz_offset_s = Ptime_clock.current_tz_offset_s () in
      let s = Time.to_human_string ?tz_offset_s x in
      Printf.sprintf "Получена: %s" s in
    let w = new Tree.Item.t
              ~text:""
              ~value:() () in
    let upd = fun (model:Time.t) -> w#item#set_text @@ to_string model in
    w, upd in
  update_time init.timestamp;
  let _e =
    React.E.map (fun (prev,model) ->
        if not @@ equal_general_info prev.general model.general
        then update_gen model.general;
        if not @@ (Equal.list equal_pid_info) prev.pids model.pids
        then update_pids model.pids;
        if not @@ (Equal.list equal_service_info) prev.services model.services
        then update_serv model.services;
        if not @@ (Equal.list equal_emm_info) prev.emm model.emm
        then update_emm model.emm;
        if not @@ (Equal.list equal_table_info) prev.tables model.tables
        then update_tabl @@ map_tables model.tables;
        if not @@ Time.equal prev.timestamp model.timestamp
        then update_time model.timestamp)
      event in
  let opt  = (serv :: tabl :: pids :: emm :: []) in
  let tree = new Tree.t ~items:(time :: gen :: opt) () in
  let ()   = tree#set_dense true in
  let ()   = tree#set_on_destroy
             @@ Some (fun () -> React.E.stop ~strong:true _e) in
  tree
