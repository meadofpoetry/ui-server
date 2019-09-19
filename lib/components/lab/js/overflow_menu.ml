open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
include Components_lab_tyxml.Overflow_menu
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

module Selector = struct
  let actions = Printf.sprintf ".%s" CSS.actions

  let overflow = Printf.sprintf ".%s" CSS.overflow

  let overflow_icon = Printf.sprintf ".%s > .%s" CSS.overflow Icon_button.CSS.root

  let menu = Printf.sprintf ".%s" Menu.CSS.root

  let icon = Printf.sprintf ".%s" Icon_button.CSS.icon
end

module Attr = struct
  let aria_label = "aria-label"

  let title = "title"
end

let name = "overflow-menu"

let fail_no_element ?base name =
  let error = Printf.sprintf "`%s` element must be present" name in
  match base with
  | None -> failwith error
  | Some s -> failwith @@ s ^ ": " ^ error

let menu_of_actions actions =
  Menu.make
    ~list:
      (Item_list.Markup_js.create
         ~children:
           (List.map
              (fun x ->
                let icon =
                  match Element.query_selector x Selector.icon with
                  | None -> None
                  | Some x ->
                      let elt = Js.Unsafe.coerce @@ x##cloneNode Js._true in
                      Element.remove_class elt Icon_button.CSS.icon;
                      Some (Js_of_ocaml_tyxml.Tyxml_js.Of_dom.of_element elt)
                in
                let name =
                  match Element.get_attribute x Attr.title with
                  | Some x -> x
                  | None -> (
                    match Element.get_attribute x Attr.aria_label with
                    | Some x -> x
                    | None ->
                        let err =
                          Printf.sprintf
                            "%s: `title` or `aria-label` attribute should be provided \
                             for every action"
                            name
                        in
                        failwith err)
                in
                Menu.Markup_js.Item_list.create_item
                  ?graphic:icon
                  ~primary_text:(`Text name)
                  ())
              actions)
         ())
    ()

class t ?(resize_handler = true) (elt : Dom_html.element Js.t) () =
  object (self)
    val overflow_elt =
      match Element.query_selector elt Selector.overflow with
      | None -> fail_no_element ~base:name Selector.overflow
      | Some x -> x

    val actions_elt =
      match Element.query_selector elt Selector.actions with
      | None -> fail_no_element ~base:name Selector.actions
      | Some x -> x

    val overflow_icon =
      match Element.query_selector elt Selector.overflow_icon with
      | None -> fail_no_element ~base:name Selector.overflow_icon
      | Some x -> Icon_button.attach x

    val mutable menu_elt = None

    val mutable listeners = []

    inherit Widget.t elt () as super

    method! init () : unit =
      let menu =
        match Element.query_selector elt Selector.menu with
        | Some x -> Menu.attach x
        | None -> menu_of_actions @@ Element.children actions_elt
      in
      Dom.insertBefore overflow_elt menu#root overflow_icon#root##.nextSibling;
      menu_elt <- Some menu;
      menu#set_quick_open true;
      menu#set_anchor_element overflow_elt;
      menu#set_anchor_corner Bottom_left;
      super#init ()

    method! initial_sync_with_dom () : unit =
      listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.(
          [ (if resize_handler
            then
              onresizes (fun _ _ ->
                  self#layout ();
                  Lwt.return_unit)
            else Lwt.return_unit)
          ; Menu.Lwt_js_events.selects (self#menu)#root (fun e _ ->
                let detail = Widget.event_detail e in
                let actions = Element.children actions_elt in
                match List.nth_opt actions detail##.index with
                | None -> Lwt.return_unit
                | Some button ->
                    Js.Opt.iter (Dom_html.CoerceTo.button button) (fun x -> x##click);
                    Lwt.return_unit)
          ; clicks overflow_icon#root self#handle_click ]
          @ listeners);
      super#initial_sync_with_dom ()

    method! layout () : unit =
      let offset_top = super#root##.offsetTop in
      let nav_items = Element.children actions_elt in
      let menu_items = Option.fold ~none:[] ~some:(fun x -> x#items) menu_elt in
      let rec loop acc = function
        | _, [] -> acc
        | [], menu ->
            List.iter (fun x -> x##.style##.display := Js.string "") menu;
            List.length menu > 0
        | nav :: tl, menu_item :: tl' ->
            let acc =
              if nav##.offsetTop > offset_top
              then (
                menu_item##.style##.display := Js.string "";
                true)
              else (
                menu_item##.style##.display := Js.string "none";
                acc)
            in
            loop acc (tl, tl')
      in
      if loop false (nav_items, menu_items)
      then overflow_elt##.style##.display := Js.string ""
      else overflow_elt##.style##.display := Js.string "none";
      super#layout ()

    method! destroy () : unit =
      List.iter Lwt.cancel listeners;
      listeners <- [];
      Option.iter Widget.destroy menu_elt;
      overflow_icon#destroy ();
      menu_elt <- None;
      super#destroy ()

    method menu : Menu.t = Option.get menu_elt

    method private handle_click e _ : unit Lwt.t =
      let target = Dom_html.eventTarget e in
      if (not @@ Element.contains (self#menu)#root target)
         && (not @@ Element.equal (self#menu)#root target)
      then (self#menu)#reveal ()
      else Lwt.return_unit
  end

let make ?classes ?attrs ?resize_handler ?overflow ?menu ~actions () : t =
  let (elt : Dom_html.element Js.t) =
    Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
    @@ Markup_js.create
         ?classes
         ?attrs
         ?overflow
         ?menu:(Option.map (fun x -> x#markup) menu)
         ~actions
         ()
  in
  new t ?resize_handler elt ()

let attach ?resize_handler (elt : #Dom_html.element Js.t) : t =
  new t ?resize_handler (Element.coerce elt) ()
