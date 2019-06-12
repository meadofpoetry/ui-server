open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Application_types
open Pipeline_types

type selected = (Stream.ID.t * ((int * int list) list)) list [@@deriving show]

module CSS = struct
  let root = "pipeline-structure"
  let checkbox_spacer = BEM.add_element root "checkbox-spacer"
end

let get_checked ?(filter_empty = false) children =
  let children =
    if filter_empty
    then List.filter (fun (x : Treeview.node) ->
        match x.children with [] -> false | _ -> true)
        children
    else children in
  let checked = match children with
    | [] -> false
    | x ->
      List.for_all (fun (x : Treeview.node) -> match x.checked with
          | None -> true | Some x -> x) x in
  let indeterminate =
    not checked
    && Utils.Option.is_some
    @@ List.find_opt (fun (x : Treeview.node) ->
        match x.checked, x.indeterminate with
        | None, None -> false
        | Some x, None | None, Some x -> x
        | Some x, Some y -> x || y)
      children in
  checked, indeterminate

let contains pattern value =
  let len = String.length value in
  if len > String.length pattern
  then false
  else
    let sub = String.sub pattern 0 len in
    String.uppercase_ascii sub = String.uppercase_ascii value

let make_pid ?(applied : Structure.pid option) (pid : Structure.pid) =
  let text = Printf.sprintf "PID %d (0x%04X), %s"
      pid.pid pid.pid pid.stream_type_name in
  (* FIXME we should match by pid type, but it is not working at the moment *)
  let checked, graphic =
    if contains pid.stream_type_name "video"
    || contains pid.stream_type_name "audio"
    then (
      let checked = Utils.Option.is_some applied in
      Some checked, (Checkbox.make ~checked ())#root)
    else (
      let elt = Dom_html.(createSpan document) in
      Element.add_class elt CSS.checkbox_spacer;
      None, elt) in
  Treeview.make_node
    ~value:(string_of_int pid.pid)
    ~graphic
    ?checked
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
  let children = List.map make ch.pids in
  let checked, indeterminate = get_checked children in
  let graphic = match ch.pids with
    | [] ->
      let elt = Dom_html.(createSpan document) in
      Element.add_class elt CSS.checkbox_spacer;
      elt
    | _ -> (Checkbox.make ~checked ~indeterminate ())#root in
  Treeview.make_node
    ~value:(string_of_int ch.number)
    ~graphic
    ~checked
    ~indeterminate
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
  let children =
    List.map make
    @@ List.sort (fun (x : Structure.channel) y ->
        compare x.number y.number) s.structure.channels in
  let checked, indeterminate = get_checked ~filter_empty:true children in
  let checkbox = Checkbox.make ~checked ~indeterminate () in
  Treeview.make_node
    ~value:(Stream.ID.to_string s.source.id)
    ~graphic:checkbox#root
    ~checked
    ~indeterminate
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

let merge_trees ~(old : Treeview.t) ~(cur : Treeview.t) =
  let active = Dom_html.document##.activeElement in
  let try_focus ~old ~cur =
    Js.Opt.to_option
    @@ Js.Opt.bind active (fun active ->
        if Element.equal old active
        then Js.some cur else Js.null) in
  List.fold_left (fun acc x ->
      match cur#node_value x with
      | None -> acc
      | Some v ->
        match List.find_opt (fun x ->
            match old#node_value x with
            | None -> false
            | Some v' -> String.equal v v') old#nodes with
        | None -> acc
        | Some node ->
          let attr = Treeview.Attr.aria_expanded in
          begin match Element.get_attribute node attr with
            | None -> ()
            | Some a -> Element.set_attribute x attr a
          end;
          match try_focus ~old:node ~cur:x with
          | None -> acc
          | Some _ as x -> x)
    None cur#nodes

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
    val mutable _treeview = treeview
    val mutable _structure : Structure.packed list = actual
    val mutable _applied : Structure.t list = applied
    val mutable _on_submit = None

    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      super#add_class CSS.root;
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
            treeview#node_value node
            >>= int_of_string_opt
            >>= fun pid -> Js.Opt.to_option @@ treeview#node_parent node
            >>= fun channel' -> treeview#node_value channel'
            >>= int_of_string_opt
            >>= fun channel -> Js.Opt.to_option @@ treeview#node_parent channel'
            >>= treeview#node_value
            >>= Stream.ID.of_string_opt
            >>= fun stream -> Some (stream, channel, pid) in
          match value with
          | None -> acc
          | Some v -> merge acc v)
        [] selected

    method notify : event -> unit = function
      | `Actual x ->
        _structure <- x;
        let treeview = make_treeview ~applied:_applied ~actual:x in
        let focus_target = merge_trees ~old:_treeview ~cur:treeview in
        super#remove_child _treeview;
        _treeview#destroy ();
        if treeview#is_empty
        then self#append_treeview placeholder
        else (
          self#append_treeview treeview;
          super#remove_child placeholder);
        Utils.Option.iter (fun x -> x##focus) focus_target;
        _treeview <- treeview
      | `Applied x ->
        _applied <- x;
        let treeview = make_treeview ~applied:x ~actual:_structure in
        let focus_target = merge_trees ~old:_treeview ~cur:treeview in
        super#remove_child _treeview;
        _treeview#destroy ();
        if treeview#is_empty
        then self#append_treeview placeholder
        else (
          self#append_treeview treeview;
          super#remove_child placeholder);
        Utils.Option.iter (fun x -> x##focus) focus_target;
        _treeview <- treeview

    method private append_treeview : 'a. (#Widget.t as 'a) -> unit =
      super#insert_child_at_idx 0
  end

let make ~applied ~actual () =
  new t ~applied ~actual ()
