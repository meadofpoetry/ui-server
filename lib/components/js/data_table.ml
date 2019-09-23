open Js_of_ocaml
open Js_of_ocaml_tyxml
include Components_tyxml.Data_table
module D = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
module R = Make (Impl.R.Xml) (Impl.R.Svg) (Impl.R.Html)

module Attr = struct
  let data_row_id = "data-row-id"

  let aria_selected = "aria-selected"
end

module Selector = struct
  let row = Printf.sprintf "tr.%s" CSS.row

  let row_checkbox = Printf.sprintf ".%s" CSS.row_checkbox

  let row_selected = Printf.sprintf ".%s" CSS.row_selected

  let header_row_checkbox = Printf.sprintf ".%s" CSS.header_row_checkbox

  let header_row = Printf.sprintf ".%s" CSS.header_row

  let content = Printf.sprintf "tbody.%s" CSS.content

  let table = Printf.sprintf "table.%s" CSS.table
end

module Event = struct
  class type selectDetail =
    object
      method rowId : Js.js_string Js.t Js.opt Js.readonly_prop

      method rowIndex : int Js.readonly_prop

      method row : Dom_html.tableRowElement Js.t Js.readonly_prop

      method selected : bool Js.t Js.readonly_prop
    end

  let select : selectDetail Js.t Dom_html.customEvent Js.t Dom_html.Event.typ =
    Dom_events.Typ.make (CSS.root ^ ":select")

  let selectall : unit Dom_html.customEvent Js.t Dom_html.Event.typ =
    Dom_events.Typ.make (CSS.root ^ ":selectall")

  let unselectall : unit Dom_html.customEvent Js.t Dom_html.Event.typ =
    Dom_events.Typ.make (CSS.root ^ ":unselectall")
end

module Lwt_js_events = struct
  open Js_of_ocaml_lwt.Lwt_js_events

  let select ?use_capture ?passive t = make_event ?use_capture ?passive Event.select t

  let selects ?cancel_handler ?use_capture ?passive t =
    seq_loop ?cancel_handler ?use_capture ?passive select t

  let selectall ?use_capture ?passive t =
    make_event ?use_capture ?passive Event.selectall t

  let selectalls ?cancel_handler ?use_capture ?passive t =
    seq_loop ?cancel_handler ?use_capture ?passive selectall t

  let unselectall ?use_capture ?passive t =
    make_event ?use_capture ?passive Event.unselectall t

  let unselectalls ?cancel_handler ?use_capture ?passive t =
    seq_loop ?cancel_handler ?use_capture ?passive unselectall t
end

let iteri_node_list f (nodes : 'a #Dom.nodeList Js.t) =
  let rec aux = function
    | i when i = nodes##.length -> ()
    | i ->
        f i (Js.Opt.get (nodes##item i) (fun () -> assert false));
        aux (succ i)
  in
  aux 0

let foldi_node_list f acc (nodes : 'a #Dom.nodeList Js.t) =
  let rec aux acc = function
    | i when i = nodes##.length -> acc
    | i ->
        let acc = f acc i (Js.Opt.get (nodes##item i) (fun () -> assert false)) in
        aux acc (succ i)
  in
  aux acc 0

class t (elt : Dom_html.element Js.t) () =
  object (self)
    inherit Widget.t elt () as super

    val header_row : Dom_html.element Js.t option =
      Element.query_selector elt Selector.header_row

    val table : Dom_html.tableElement Js.t =
      Js.Unsafe.coerce (Element.query_selector_exn elt Selector.table)

    val mutable header_row_checkbox : Checkbox.t option = None

    val mutable listeners = []

    val mutable row_checkboxes : Checkbox.t array = [||]

    method! init () : unit = super#init ()

    method! initial_sync_with_dom () : unit =
      let ( ^:: ) x l =
        match x with
        | None -> l
        | Some x -> x :: l
      in
      listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.(
          Option.map
            (fun row -> changes row self#handle_header_row_checkbox_change)
            header_row
          ^:: [changes table self#handle_row_checkbox_change]
          @ listeners);
      self#layout ();
      super#initial_sync_with_dom ()

    method! layout () : unit =
      if self#are_rows_selectable ()
      then (
        self#register_header_row_checkbox ();
        self#register_row_checkboxes ();
        self#set_header_row_checkbox_state ());
      super#layout ()

    method! destroy () : unit =
      Option.iter Widget.destroy header_row_checkbox;
      super#destroy ()

    method is_empty : bool =
      match self#rows with
      | [] -> true
      | _ -> false

    method dense : bool = super#has_class CSS.dense

    method set_dense (x : bool) : unit = super#toggle_class ~force:x CSS.dense

    method table : Dom_html.tableElement Js.t = table
    (** Underlying [tableElement]. *)

    method tbodies : Dom_html.tableSectionElement Js.t list =
      Dom.list_of_nodeList (table##.tBodies :> _ Dom.nodeList Js.t)
    (** Proxy to the [tableElement##.tBodies] property. *)

    method tbody : Dom_html.tableSectionElement Js.t =
      match self#tbodies with
      | [] -> raise Not_found
      | hd :: _ -> hd
    (** First available [tBody] element. *)

    method rows : Dom_html.tableRowElement Js.t list =
      Dom.list_of_nodeList (self#rows_collection :> _ Dom.nodeList Js.t)
    (** List of row elements excluding the header row. *)

    method rows_collection : Dom_html.tableRowElement Dom_html.collection Js.t =
      (* FIXME if support for multiple bodies is considered, this needs to be fixed *)
      self#tbody##.rows
    (** Proxy to the [tableElement##.rows] property. *)

    method private notify_row_selection_changed ~row ~index ~selected () : unit =
      let detail : Event.selectDetail Js.t =
        object%js
          val row = row

          val rowId = row##getAttribute (Js.string Attr.data_row_id)

          val rowIndex = index

          val selected = Js.bool selected
        end
      in
      super#emit ~should_bubble:true ~detail Event.select
    (** Notifies when row selection is changed. *)

    method private notify_selected_all () : unit =
      super#emit ~should_bubble:true Event.selectall
    (** Notifies when header row is checked. *)

    method private notify_unselected_all () : unit =
      super#emit ~should_bubble:true Event.unselectall
    (** Notifies when header row is unchecked. *)

    method private get_row_by_child_element elt : Dom_html.tableRowElement Js.t Js.opt =
      Js.Opt.bind (Element.closest elt Selector.row) (fun row ->
          Dom_html.CoerceTo.tr row)
    (** Returns row element that contains given child element, if any. *)

    method private get_selected_row_count () : int =
      foldi_node_list
        (fun acc _ item ->
          if Element.has_class item CSS.row_selected then acc + 1 else acc)
        0
        self#rows_collection
    (** Selected row count. *)

    method private handle_header_row_checkbox_change _ _ : unit Lwt.t =
      let is_checked =
        Option.fold
          ~none:false
          ~some:(fun checkbox -> checkbox#checked)
          header_row_checkbox
      in
      iteri_node_list
        (fun index row ->
          let checkbox = row_checkboxes.(index) in
          checkbox#toggle ~force:is_checked ();
          self#select_row ~selected:is_checked row)
        self#rows_collection;
      if is_checked then self#notify_selected_all () else self#notify_unselected_all ();
      Lwt.return_unit
    (** Handles header checkbox change event. *)

    method private handle_row_checkbox_change e _ : unit Lwt.t =
      let target = Dom.eventTarget e in
      let row = self#get_row_by_child_element target in
      Js.Opt.case row Lwt.return (fun row ->
          let index = row##.rowIndex in
          self#set_header_row_checkbox_state ();
          self#notify_row_selection_changed
            ~row
            ~index
            ~selected:(row_checkboxes.(index))#checked
            ();
          Lwt.return_unit)
    (** Handles change event originated from row checkboxes. *)

    method private set_header_row_checkbox_state () =
      match header_row_checkbox with
      | None -> ()
      | Some checkbox ->
          let selected_rows = self#get_selected_row_count () in
          let total = self#rows_collection##.length in
          if selected_rows = total
          then (
            checkbox#toggle ~force:true ();
            checkbox#set_indeterminate false)
          else (
            checkbox#set_indeterminate (selected_rows <> 0);
            checkbox#toggle ~force:false ())
    (** Updates header row checkbox state based on number of rows selected. *)

    method private select_row ~selected row =
      if selected
      then (
        Element.add_class row CSS.row_selected;
        Element.set_attribute row Attr.aria_selected "true")
      else (
        Element.remove_class row CSS.row_selected;
        Element.set_attribute row Attr.aria_selected "false")

    method private are_rows_selectable () : bool =
      Option.is_some (Element.query_selector super#root Selector.row_checkbox)
    (** Returns [true] if table rows are selectable. *)

    method private register_header_row_checkbox () : unit =
      Option.iter Widget.destroy header_row_checkbox;
      let checkbox = Element.query_selector super#root Selector.header_row_checkbox in
      header_row_checkbox <- Option.map Checkbox.attach checkbox
    (** Initializes header row checkbox. Destroys previous header row checkbox
        instance if any. *)

    method private register_row_checkboxes () : unit =
      Array.iter Widget.destroy row_checkboxes;
      let checkboxes =
        Array.map (fun row ->
            Checkbox.attach @@ Element.query_selector_exn row Selector.row_checkbox)
        @@ Array.of_list self#rows
      in
      row_checkboxes <- checkboxes
    (** Initializes all row checkboxes. Destroys previous
        row checkboxes instances if any. This is usually called when
        row checkboxes are added or removed from the table. *)
  end

let attach (elt : #Dom_html.element Js.t) : t = new t (elt :> Dom_html.element Js.t) ()

let make ?classes ?a ?dense ?children () =
  D.data_table ?classes ?a ?dense ?children () |> Tyxml_js.To_dom.of_div |> attach
