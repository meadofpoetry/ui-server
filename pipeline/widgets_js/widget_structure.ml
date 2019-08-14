open Js_of_ocaml
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
    && Option.is_some
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

let make_pid ((state : Structure.Annotated.state), (pid : Structure.pid)) =
  let text = Printf.sprintf "PID %d (0x%04X), %s"
      pid.pid pid.pid pid.stream_type_name in
  (* FIXME we should match by pid type, but it is not working at the moment *)
  let checked, graphic =
    if contains pid.stream_type_name "video"
    || contains pid.stream_type_name "audio"
    then (
      let checked = match state with
        | `Stored | `Active_and_stored -> true
        | `Avail -> false in
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

let make_channel ((_state : Structure.Annotated.state),
                  (ch : Structure.Annotated.channel)) =
  let service_name = match ch.service_name with
    | "" -> Printf.sprintf "Программа %d" ch.number
    | s -> s in
  let text = match ch.provider_name with
    | "" -> service_name
    | s -> Printf.sprintf "%s (%s)" service_name s in
  let children = List.map make_pid ch.pids in
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

let make_stream ((_state : Structure.Annotated.state),
                 ({ id; channels; _ } : Structure.Annotated.structure)) =
  (* FIXME stream name *)
  let text = Printf.sprintf "%s" @@ Stream.ID.to_string id in
  let compare (_, (a : Structure.Annotated.channel as 'a)) (_, (b : 'a)) =
    compare a.number b.number in
  let children = List.map make_channel @@ List.sort compare channels in
  let checked, indeterminate = get_checked ~filter_empty:true children in
  let checkbox = Checkbox.make ~checked ~indeterminate () in
  Treeview.make_node
    ~value:(Stream.ID.to_string id)
    ~graphic:checkbox#root
    ~checked
    ~indeterminate
    ~children
    text

let make_treeview (structure : Structure.Annotated.t) =
  let nodes = List.map make_stream structure in
  Treeview.make ~dense:true nodes

type event =
  [ `Structure of Structure.Annotated.t
  ]

let merge acc ((stream : Stream.ID.t), (chan : int), (pid : int)) =
  Utils.List.Assoc.update ~eq:Stream.ID.equal (function
      | None -> Some [chan, [pid]]
      | Some x -> Some (Utils.List.Assoc.update ~eq:(=) (function
          | None -> Some [pid]
          | Some x -> Some (pid :: x)) chan x))
    stream acc

let merge_with_structures
    (structures : Structure.Annotated.t)
    (selected : selected) : Structure.t list =
  let open Structure.Annotated in
  Utils.List.filter_map (fun (id, channels) ->
      match List.find_opt (fun (_, (s : structure)) ->
          Stream.ID.equal id s.id) structures with
      | None -> None
      | Some (_, (x : structure)) ->
        let channels = Utils.List.filter_map (fun (chan, pids) ->
            match List.find_opt (fun (_, (ch : channel)) ->
                ch.number = chan) x.channels with
            | None -> None
            | Some (_, (ch : channel)) ->
              match Utils.List.filter_map (fun (_, (pid : Structure.pid)) ->
                  if List.mem pid.pid pids
                  then Some pid else None) ch.pids with
              | [] -> None
              | pids -> Some { Structure.
                               number = ch.number
                             ; provider_name = ch.provider_name
                             ; service_name = ch.service_name
                             ; pids
                             }) channels in
        match channels with
        | [] -> None
        | channels ->
          Some { Structure.
                 id = x.id
               ; uri = x.uri
               ; channels
               })
    selected

let merge_trees ~(old : Treeview.t) ~(cur : Treeview.t) =
  let active = Dom_html.document##.activeElement in
  let try_focus ~old ~cur =
    Js.Opt.to_option
    @@ Js.Opt.bind active (fun active ->
        if Element.equal old active
        then Js.some cur else Js.null) in
  let rec merge acc old_nodes cur_nodes =
    List.fold_left (fun acc x ->
        match cur#node_value x with
        | None -> acc
        | Some v ->
          match List.find_opt (fun x ->
              match old#node_value x with
              | None -> false
              | Some v' -> String.equal v v') old_nodes with
          | None -> acc
          | Some node ->
            let attr = Treeview.Attr.aria_expanded in
            begin match Element.get_attribute node attr with
              | None -> ()
              | Some a -> Element.set_attribute x attr a
            end;
            let acc = match try_focus ~old:node ~cur:x with
              | None -> acc
              | Some _ as x -> x in
            merge acc
              (old#node_children node)
              (cur#node_children x))
      acc cur_nodes in
  merge None old#root_nodes cur#root_nodes

class t (structure : Structure.Annotated.t) () =
  let submit = Button.make
      ~label:"Применить"
      () in
  let buttons = Card.Actions.make_buttons [submit]; in
  let actions = Card.Actions.make [buttons] in
  object(self)
    val placeholder =
      Ui_templates.Placeholder.With_icon.make
        ~text:"Потоки не обнаружены"
        ~icon:Icon.SVG.(make_simple Path.information)#root
        ()
    val mutable _treeview = make_treeview structure
    val mutable _structure : Structure.Annotated.t = structure
    val mutable _on_submit = None

    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      super#add_class CSS.root;
      super#add_class Box.CSS.root;
      super#add_class Box.CSS.vertical;
      if _treeview#is_empty
      then super#append_child placeholder
      else super#append_child _treeview;
      super#append_child actions;
      _on_submit <- Some (Events.clicks submit#root (fun _ _ ->
          Lwt.map ignore @@ self#submit ()))

    method! destroy () : unit =
      super#destroy ();
      submit#destroy ();
      placeholder#destroy ();
      buttons#destroy ();
      actions#destroy ();
      Option.iter Lwt.cancel _on_submit;
      _on_submit <- None

    method submit () : (unit, string) Lwt_result.t =
      let req = Pipeline_http_js.Http_structure.apply_structures self#value in
      submit#set_loading_lwt req;
      Lwt_result.map_err Api_js.Http.error_to_string req

    method value : Structure.Many.t =
      let selected = _treeview#selected_leafs in
      print_endline @@ Printf.sprintf "selected: %d" @@ List.length selected;
      merge_with_structures _structure
      @@ List.fold_left (fun acc node ->
          let ( >>= ) x f = match x with None -> None | Some x -> f x in
          let value =
            _treeview#node_value node
            >>= int_of_string_opt
            >>= fun pid -> Js.Opt.to_option @@ _treeview#node_parent node
            >>= fun channel' -> _treeview#node_value channel'
            >>= int_of_string_opt
            >>= fun channel -> Js.Opt.to_option @@ _treeview#node_parent channel'
            >>= _treeview#node_value
            >>= Stream.ID.of_string_opt
            >>= fun stream -> Some (stream, channel, pid) in
          match value with
          | None -> acc
          | Some v -> merge acc v)
        [] selected

    method notify : event -> unit = function
      | `Structure x ->
        _structure <- x;
        let old = _treeview in
        let cur = make_treeview x in
        let focus_target = merge_trees ~old ~cur in
        super#remove_child old;
        old#destroy ();
        if cur#is_empty
        then self#append_treeview placeholder
        else (
          self#append_treeview cur;
          super#remove_child placeholder);
        Option.iter (fun x -> x##focus) focus_target;
        _treeview <- cur

    method private append_treeview : 'a. (#Widget.t as 'a) -> unit =
      super#insert_child_at_idx 0
  end

let make (structure : Structure.Annotated.t) () =
  new t structure ()
