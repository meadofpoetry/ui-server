open Containers
open Components
open Common
open Board_types.Streams.TS
open Lwt_result.Infix
open Api_js.Api_types

type dumpable =
  { name : string * string
  ; get  : unit -> (section,string) Lwt_result.t
  ; prev : section option ref
  }

type config =
  { stream : Stream.t
  }

let name = "Таблицы"

let settings = None

let base_class = "qos-niit-tables"

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
type tree = unit Tree.t

module Table = struct

  module Id = struct
    type t     =
      { id        : int
      ; id_ext    : int
      ; section   : int
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

  let req_of_table (model:model) =
    let control = model.control in
    let table_id_ext = model.table.id_ext in
    let eit_params = model.table.eit_params in
    let r = Requests.Streams.HTTP.get_si_psi_section
              ~id:model.stream
              ~table_id:model.table.id
              ~section:model.table.section in
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

  let to_table_name table section =
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

  let widget      = fun w -> w#widget
  let equal_model = fun x1 x2 -> equal_table_info x1.table x2.table
  let id_of_model = fun (x:model)  ->
    let open Id in
    { id        = x.table.id
    ; section   = x.table.section
    ; version   = x.table.version
    ; id_ext    = x.table.id_ext
    ; eit_ts_id = x.table.eit_params.ts_id
    ; eit_nw_id = x.table.eit_params.orig_nw_id }

  let make (init:model) =
    let bytes, update_bytes =
      let to_string x =
        let s = if x > 1 && x < 5 then "байта" else "байт" in
        Printf.sprintf "%d %s" x s in
      let w = new Typography.Text.t ~text:"" () in
      let v = { get = (fun (x:model) -> x.table.length)
              ; eq  = Int.equal
              ; upd = (w#set_text % to_string) } in
      w, v in
    let prev   = ref init in
    let leaf   = new Tree.Item.t
                   ~text:""
                   ~secondary_text:""
                   ~meta:bytes#widget
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
                        x.table.id, x.table.last_section)
      ; eq  = (fun (pid1, ver1, id1, lsn1)
                   (pid2, ver2, id2, lsn2) ->
        pid1 = pid2 && ver1 = ver2 && id1 = id2 && lsn1 = lsn2)
      ; upd = (fun (pid, ver, id, lsn) ->
        let s = to_secondary pid ver id lsn in
        leaf#item#set_secondary_text s)
      } in
    Dom_events.listen leaf#item#root Dom_events.Typ.click (fun _ _ ->
        let name = to_table_name init.table !prev.table.section in
        let prev' = ref None in
        let get  = fun () ->
          req_of_table !prev
          >|= (fun dump -> prev' := Some dump; dump)
          |> Lwt_result.map_err Api_js.Requests.err_to_string in
        let dumpable = { name; get; prev = prev' } in
        init.push dumpable; true) |> ignore;
    let update = fun ?(previous:model option) (model:model) ->
      setter ?previous model update_primary;
      setter ?previous model update_secondary;
      setter ?previous model update_bytes in
    update init;
    leaf, fun x -> update ~previous:!prev x

end

module Tables =
  Make_array(struct
      type widget = tree
      module Node = Table

      let root (w:widget) = w#root
      let make (_:Node.model list) =
        let tree   = new Tree.t ~two_line:true ~items:[] () in
        let ()     = tree#set_dense true in
        tree, (fun _ -> ())
    end)

let make_stream (id:Stream.id)
      (init:  tables option)
      (event: tables React.event)
      (push:  dumpable -> unit)
      control =
  let map_tables =
    let open Table in
    List.map (fun x -> { stream = id; push; control; table = x }) in
  let init_list = match init with None -> [] | Some x -> x.tables in
  let event : (table_info list * table_info list) React.event =
    React.S.diff (fun n o -> o, n)
    @@ React.S.hold ~eq:(Equal.list equal_table_info) init_list
    @@ React.E.map (fun (x:tables) -> x.tables) event in
  let tree, update_tree = Tables.make (map_tables init_list) in
  let _e =
    React.E.map (fun ((prev:table_info list),
                      (model:table_info list)) ->
        if not @@ (Equal.list equal_table_info) prev model
        then update_tree @@ map_tables model)
      event in
  let ()   = tree#set_dense true in
  let ()   = tree#set_on_destroy
             @@ Some (fun () -> React.E.stop ~strong:true _e) in
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
        let text   = Dom_html.createPre Dom_html.document
                     |> Widget.create in
        let err x  = Ui_templates.Placeholder.create_with_error ~text:x () in
        let ph  x  = Ui_templates.Placeholder.create_with_icon
                       ~icon:"info"
                       ~text:x () in
        let upd = function
          | Some { section; parsed = Some x; _ } ->
             parsed#set_empty ();
             text#set_text_content (Yojson.Safe.pretty_to_string x);
             Dom.appendChild parsed#root text#root;
             push section
          | Some { section; parsed = None; _ } ->
             parsed#set_empty ();
             Dom.appendChild parsed#root
               (ph "Не удалось разобрать содержимое секции")#root;
             push section
          | None     ->
             parsed#set_empty ();
             Dom.appendChild parsed#root (ph "Нет захваченных данных")#root;
             push "" in
        let get    = fun () ->
          Lwt.catch (fun () ->
              get ()
              >|= (function
                   | Ok _    -> parsed#set_empty ();
                                upd !prev;
                   | Error s -> parsed#set_empty ();
                                Dom.appendChild parsed#root (err s)#root))
            (fun e ->
              parsed#set_empty ();
              Dom.appendChild parsed#root (err @@ Printexc.to_string e)#root;
              Lwt.return_unit) in
        upd !prev;
        let () = button#set_getter (Some get) in
        let () = title#set_text @@ fst name in
        let () = subtitle#set_text @@ snd name in
        let () = button#set_disabled false in
        ()) event
    |> Lwt_react.E.keep in
  let vsplit = new Vsplit.t parsed hexdump () in
  let vbox   = new Vbox.t ~widgets:[ header
                                   ; (new Divider.t ())#widget
                                   ; vsplit#widget
                                   ; (new Divider.t ())#widget
                                   ; options#widget ] () in
  vbox#add_class base_class;
  vbox#widget

class t ~(config:config)
        ~(init:tables option)
        ~(event:tables React.event)
        (control:int)
        () =
  let stream_panel_class = Markup.CSS.add_element base_class "stream-panel" in
  let id  = match config.stream.id with
    | `Ts id -> id
    | `Ip _  -> failwith "UDP" in
  let box = Dom_html.createDiv Dom_html.document
            |> Widget.create in
  let e_dumpable, push = React.E.create () in
  let make_struct init =
    let wdg = make_stream id init event push control in
    wdg in
  let tables = make_struct init in
  let dump = make_dump e_dumpable in
  object(self)
    inherit Hsplit.t box dump ()

    initializer
      Dom.appendChild box#root tables#root;
      box#add_class stream_panel_class;
      self#add_class base_class
  end

let make ~(config:config)
      control =
  let id  = match config.stream.id with
    | `Ts id -> id
    | `Ip _  -> failwith "UDP" in
  let init =
    Requests.Streams.HTTP.get_tables ~id ~limit:1 control
    >>= (function
         | Raw s ->
            (match List.head_opt s.data with
             | Some (_, tables) -> Some tables
             | None -> None)
            |> Lwt_result.return
         | _     -> Lwt.fail_with "got compressed")
    |> Lwt_result.map_err Api_js.Requests.err_to_string in
  let loader =
    init
    >|= (fun init ->
      let e, sock = Requests.Streams.WS.get_tables ~id control in
      new t ~config ~init ~event:e control ())
    >|= Widget.coerce
    |> Ui_templates.Loader.create_widget_loader
  in loader
