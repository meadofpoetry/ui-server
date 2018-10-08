open Containers
open Tyxml_js

module Markup = Components_markup.Tab_scroller.Make(Xml)(Svg)(Html)

type align =
  | Start
  | End
  | Center

let horizontal_scroll_height = ref None

let compute_horizontal_scroll_height ?(cache = true) () : int option =
  match cache, !horizontal_scroll_height with
  | true, Some x -> Some x
  | _ ->
     let el = Dom_html.(createDiv document) in
     el##.classList##add (Js.string Markup.scroll_test_class);
     Dom.appendChild Dom_html.document##.body el;
     let height = el##.offsetHeight - el##.clientHeight in
     if cache then horizontal_scroll_height := Some height;
     Dom.removeChild Dom_html.document##.body el;
     Some height

class ['a, 'b] t ?on_change ?align
        ~(tabs:('a, 'b) Tab.t list) () =
  let eq = Widget.equal in
  let tabs' = List.map Widget.to_markup tabs in
  let content =
    Markup.create_scroll_content tabs' ()
    |> To_dom.of_element
    |> Widget.create in
  let area =
    Markup.create_scroll_area ~content:(Widget.to_markup content) ()
    |> To_dom.of_element
    |> Widget.create in
  let elt =
    Markup.create ~scroll_area:(Widget.to_markup area) ()
    |> To_dom.of_element in
  let s_active, set_active = React.S.create ~eq:(Equal.option eq) None in
  object(self)

    val mutable _tabs : ('a, 'b) Tab.t list = tabs
    val mutable _align : align option = align

    inherit Widget.t elt ()

    method s_active_tab : ('a, 'b) Tab.t option React.signal =
      s_active

    method active_tab : ('a, 'b) Tab.t option =
      React.S.value self#s_active_tab

    method set_active_tab (tab : ('a, 'b) Tab.t) : unit =
      set_active @@ Some tab;
      self#layout ()

    method remove_tab (tab : ('a, 'b) Tab.t) : unit =
      match List.find_opt (eq tab) self#tabs with
      | None -> ()
      | Some tab ->
         self#remove_child tab;
         _tabs <- List.remove ~eq ~x:tab self#tabs;
         self#layout ()

    method append_tab (tab : ('a, 'b) Tab.t) : unit =
      _tabs <- tabs @ [tab];
      self#append_child tab;
      self#layout ()

    method insert_tab_at_index (i : int) (tab : ('a, 'b) Tab.t) : unit =
      _tabs <- List.insert_at_idx i tab _tabs;
      self#insert_child_at_idx i tab;
      self#layout ()

    method align : align option = _align

    method set_align (x : align option) : unit =
      _align <- x;
      let pre = Components_markup.CSS.add_modifier
                  Markup.base_class "align-" in
      List.iter self#remove_class @@ self#find_classes pre;
      match x with
      | None -> ()
      | Some Start -> self#add_class Markup.align_start_class
      | Some End -> self#add_class Markup.align_end_class
      | Some Center -> self#add_class Markup.align_center_class

    method tabs = _tabs

    method area = area

    method content = content

    initializer
      area#style##.marginBottom :=
        (match compute_horizontal_scroll_height () with
         | Some x -> Js.string (Printf.sprintf "-%dpx" x)
         | None -> Js.string "");
      area#add_class Markup.scroll_area_scroll_class;
      self#set_align _align

  end
