open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Application_types
open Pipeline_types

type selected = (Stream.ID.t * ((int * int list) list)) list [@@deriving show]

let make_pid ?(applied : Structure.pid option) (pid : Structure.pid) =
  let text = Printf.sprintf "PID %d (0x%04X), %s"
      pid.pid pid.pid pid.stream_type_name in
  let checkbox = Checkbox.make
      ~checked:(Utils.Option.is_some applied)
      () in
  Treeview.make_node
    ~value:(string_of_int pid.pid)
    ~graphic:checkbox#root
    text

let make_channel ?(applied : Structure.channel option) (ch : Structure.channel) =
  let make (pid : Structure.pid) =
    let applied = match applied with
      | None -> None
      | Some v -> List.find_opt (fun (x : Structure.pid) -> x.pid = pid.pid) v.pids
    in make_pid ?applied pid
  in
  let service_name = match ch.service_name with
    | "" -> Printf.sprintf "Программа %d" ch.number
    | s -> s in
  let text = match ch.provider_name with
    | "" -> service_name
    | s -> Printf.sprintf "%s (%s)" service_name s in
  let checkbox = Checkbox.make
      ~checked:(Utils.Option.is_some applied)
      () in
  let children = List.map make ch.pids in
  Treeview.make_node
    ~value:(string_of_int ch.number)
    ~graphic:checkbox#root
    ~children
    text

let make_stream ?(applied : Structure.t option) (s : Structure.packed) =
  let make (chan : Structure.channel) =
    let applied = match applied with
      | None -> None
      | Some v -> List.find_opt (fun (c : Structure.channel) ->
          c.number = chan.number) v.channels
    in make_channel ?applied chan
  in
  let text =
    Printf.sprintf "%s"
    @@ Stream.Source.to_string s.source.source.info in
  let checkbox = Checkbox.make
      ~checked:(Utils.Option.is_some applied)
      () in
  let children =
    List.map make
    @@ List.sort (fun (x : Structure.channel) y ->
        compare x.number y.number) s.structure.channels in
  Treeview.make_node
    ~value:(Stream.ID.to_string s.source.id)
    ~graphic:checkbox#root
    ~children
    text

let make_treeview
    ~(applied : Structure.t list)
    ~(actual : Structure.packed list) =
  let make (s : Structure.packed) =
    let open Structure in
    make_stream s
      ?applied:(List.find_opt (fun x -> Uri.equal x.uri s.structure.uri) applied)
  in
  let nodes = List.map make actual in
  Treeview.make ~dense:true nodes

type event =
  [ `Applied of Structure.t list
  | `Actual of Structure.packed list
  ]

let merge acc ((stream : Stream.ID.t), (chan : int), (pid : int)) =
  Utils.List.Assoc.update ~eq:Stream.ID.equal (function
      | None -> Some [chan, [pid]]
      | Some x -> Some (Utils.List.Assoc.update ~eq:(=) (function
          | None -> Some [pid]
          | Some x -> Some (pid :: x)) chan x))
    stream acc

let merge_with_structures
    (structures : Structure.packed list)
    (selected : selected) : Structure.t list =
  Utils.List.filter_map (fun (id, channels) ->
      match List.find_opt (fun (s : Structure.packed) ->
          Stream.ID.equal id s.source.id) structures with
      | None -> None
      | Some (x : Structure.packed) ->
        let channels = Utils.List.filter_map (fun (chan, pids) ->
            match List.find_opt (fun (ch : Structure.channel) ->
                ch.number = chan) x.structure.channels with
            | None -> None
            | Some (ch : Structure.channel) ->
              match List.filter (fun (pid : Structure.pid) ->
                  List.mem pid.pid pids) ch.pids with
              | [] -> None
              | pids -> Some { ch with pids }) channels in
        match channels with
        | [] -> None
        | channels -> Some { x.structure with channels })
    selected

class t ~applied ~actual () =
  let treeview = make_treeview ~applied ~actual in
  let submit = Button.make
      ~label:"Применить"
      () in
  let buttons = Card.Actions.make_buttons [submit]; in
  let actions = Card.Actions.make [buttons] in
  object(self)
    val placeholder =
      Ui_templates.Placeholder.With_icon.make
        ~text:"Потоки не обнаружены"
        ~icon:Icon.SVG.(make_simple Path.information)
        ()
    val mutable _structure : Structure.packed list = actual
    val mutable _on_submit = None

    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      super#add_class Box.CSS.root;
      super#add_class Box.CSS.vertical;
      if treeview#is_empty
      then super#append_child placeholder
      else super#append_child treeview;
      super#append_child actions;
      _on_submit <- Some (Events.clicks submit#root (fun _ _ ->
          Lwt.map ignore @@ self#submit ()))

    method! destroy () : unit =
      super#destroy ();
      submit#destroy ();
      placeholder#destroy ();
      buttons#destroy ();
      actions#destroy ();
      Utils.Option.iter Lwt.cancel _on_submit;
      _on_submit <- None

    method submit () : (unit, string) Lwt_result.t =
      let req = Pipeline_http_js.Http_structure.apply_streams self#value in
      submit#set_loading_lwt req;
      Lwt_result.map_err Api_js.Http.error_to_string req

    method value : Structure.t list =
      let selected = treeview#selected_leafs in
      merge_with_structures _structure
      @@ List.fold_left (fun acc node ->
          let ( >>= ) x f = match x with None -> None | Some x -> f x in
          let value =
            treeview#get_node_value node
            >>= int_of_string_opt
            >>= fun pid -> Js.Opt.to_option @@ treeview#get_node_parent node
            >>= fun channel' -> treeview#get_node_value channel'
            >>= int_of_string_opt
            >>= fun channel -> Js.Opt.to_option @@ treeview#get_node_parent channel'
            >>= treeview#get_node_value
            >>= Stream.ID.of_string_opt
            >>= fun stream -> Some (stream, channel, pid) in
          match value with
          | None -> acc
          | Some v -> merge acc v)
        [] selected

    method notify : event -> unit = function
      | `Actual x -> _structure <- x
      | `Applied _ -> ()
  end

let make ~applied ~actual () =
  new t ~applied ~actual ()
