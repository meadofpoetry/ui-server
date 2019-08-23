open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components

include Components_lab_tyxml.Overflow_menu
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

module Selector = struct
  let actions = Printf.sprintf ".%s" CSS.actions
  let overflow = Printf.sprintf ".%s" CSS.overflow
  let menu = Printf.sprintf ".%s" Menu.CSS.root
end

let name = "overflow-menu"

let fail_no_element ?base name =
  let error = Printf.sprintf "`%s` element must be present" name in
  match base with
  | None -> failwith error
  | Some s -> failwith @@ s ^ ": " ^ error

class t ?(resize_handler = true)
    (elt : Dom_html.element Js.t)
    () = object(self)

  val menu = match Element.query_selector elt Selector.menu with
    | None -> fail_no_element ~base:name Selector.menu
    | Some x -> Menu.attach x
  val overflow = match Element.query_selector elt Selector.overflow with
    | None -> fail_no_element ~base:name Selector.overflow
    | Some x -> x
  val actions = match Element.query_selector elt Selector.actions with
    | None -> fail_no_element ~base:name Selector.actions
    | Some x -> x

  val mutable _listeners = []

  inherit Widget.t elt () as super

  method! init () : unit =
    menu#set_quick_open true;
    menu#set_anchor_element overflow;
    menu#set_anchor_corner Bottom_left;
    super#init ()

  method! initial_sync_with_dom () : unit =
    _listeners <- Events.(
        [ (if resize_handler
           then onresizes (fun _ _ -> self#layout (); Lwt.return_unit)
           else Lwt.return_unit)
        ; clicks overflow self#handle_click
        ]);
    super#initial_sync_with_dom ()

  method! layout () : unit =
    let offset_top = super#root##.offsetTop in
    let nav_items = Element.children actions in
    let menu_items = menu#items in
    let rec loop acc = function
      | _, [] -> acc
      | [], menu ->
        List.iter (fun x -> x##.style##.display := Js.string "") menu;
        List.length menu > 0
      | nav :: tl, menu_item :: tl' ->
        let acc =
          if nav##.offsetTop > offset_top
          then (menu_item##.style##.display := Js.string ""; true)
          else (menu_item##.style##.display := Js.string "none"; acc) in
        loop acc (tl, tl') in
    if loop false (nav_items, menu_items)
    then overflow##.style##.display := Js.string ""
    else overflow##.style##.display := Js.string "none";
    super#layout ()

  method! destroy () : unit =
    List.iter Lwt.cancel _listeners;
    _listeners <- [];
    menu#destroy ();
    super#destroy ()

  method menu : Menu.t = menu

  method private handle_click e _ : unit Lwt.t =
    let target = Dom_html.eventTarget e in
    if not @@ Element.contains menu#root target
    && not @@ Element.equal menu#root target
    then menu#reveal ()
    else Lwt.return_unit

end

let make
    ?resize_handler
    ~(menu : Menu.t)
    ~(overflow : Dom_html.element Js.t)
    ~(actions : Dom_html.element Js.t list)
    () : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create
      ~menu:menu#markup
      ~actions:(List.map Tyxml_js.Of_dom.of_element actions)
      ~overflow:(Tyxml_js.Of_dom.of_element overflow)
      () in
  new t ?resize_handler elt ()

let attach ?resize_handler (elt : #Dom_html.element Js.t) : t =
  new t ?resize_handler (Element.coerce elt) ()