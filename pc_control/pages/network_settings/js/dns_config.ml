open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Components
open Pc_control_types

let ( % ) f g x = f (g x)

let ( >>= ) = Lwt.bind

let name = "DNS"

let make_dialog () =
  let accept =
    Button.attach
    @@ Dialog.make_action ~action:Accept ~label:"Добавить" () in
  let check_input input = match input#value with
    | None -> accept#set_disabled true
    | Some _ -> accept#set_disabled false in
  let address =
    Textfield.make_textfield
      ~on_input:(fun _ x -> check_input x; Lwt.return_unit)
      ~label:"IP адрес"
      Util.ipv4_validation in
  let title =
    Tyxml_js.To_dom.of_element
    @@ Dialog.Markup.create_title_simple ~title:"Добавление DNS сервера" () in
  let content =
    Tyxml_js.To_dom.of_element
    @@ Dialog.Markup.create_content ~content:[address#markup] () in
  let actions =
    Dialog.[ Element.coerce (make_action ~action:Close ~label:"Отмена" ())
           ; accept#root
           ] in
  let dialog = Dialog.make ~title ~content ~actions () in
  dialog,
  (fun () ->
     check_input address;
     dialog#open_await ()
     >>= function
     | Close | Destroy | Custom _ -> Lwt.return_none
     | Accept -> Lwt.return address#value)

let failwith s = failwith @@ Printf.sprintf "%s: %s" name s

module Selector = struct
  let dns_list = Printf.sprintf ".%s" Item_list.CSS.root
  let button = Printf.sprintf ".%s" Card.CSS.action
end

class t (elt : Dom_html.element Js.t) = object(self)

  val dns_list : Item_list.t =
    match Element.query_selector elt Selector.dns_list with
    | None -> failwith "DNS list element not found"
    | Some x -> Item_list.attach x
  val add : Button.t =
    match Element.query_selector elt Selector.button with
    | None -> failwith "Add button element not found"
    | Some x -> Button.attach x
  val dialog : Dialog.t * (unit -> Ipaddr.V4.t option Lwt.t) = make_dialog ()

  val mutable _ripples = []
  val mutable _listeners = []

  inherit Widget.t elt () as super

  method! init () : unit =
    _ripples <- List.map (fun x -> x, Ripple.attach x) dns_list#items;
    Dom.appendChild Dom_html.document##.body (fst dialog)#root;
    super#init ()

  method! initial_sync_with_dom () : unit =
    _listeners <- Lwt_js_events.(
        [ seq_loop (make_event Item_list.Event.action) super#root
            self#handle_item_action
        ; clicks add#root (fun _ _ ->
              (snd dialog) ()
              >>= function
              | None -> Lwt.return_unit
              | Some addr -> self#append_address addr; Lwt.return_unit)
        ]);
    super#initial_sync_with_dom ()

  method! destroy () : unit =
    Element.remove_child_safe Dom_html.document##.body (fst dialog)#root;
    add#destroy ();
    (fst dialog)#destroy ();
    dns_list#destroy ();
    List.iter (Ripple.destroy % snd) _ripples;
    _ripples <- [];
    super#destroy ()

  method set_value (x : Ipaddr.V4.t list) =
    let rec aux = function
      | [], addr -> List.iter self#append_address addr
      | items, [] -> List.iter self#remove_item items
      | item :: x, addr :: y ->
        let s = Ipaddr.V4.to_string addr in
        let children = Dom.list_of_nodeList item##.childNodes in
        let text = List.find (fun x -> match x##.nodeType with
            | Dom.TEXT -> true
            | _ -> false) children in
        (Js.Unsafe.coerce text)##.textContent := Js.some @@ Js.string s;
        aux (x, y) in
    aux (dns_list#items, x)

  method value : Ipaddr.V4.t list =
    let result_to_option = function
      | Ok x -> Some x | Error _ -> None in
    Utils.List.filter_map (fun (x : Dom_html.element Js.t) ->
        Js.Opt.case x##.textContent
          (fun () -> None)
          (result_to_option % Ipaddr.V4.of_string % Js.to_string))
      dns_list#items

  method private append_address (x : Ipaddr.V4.t) =
    let item =
      Tyxml_js.To_dom.of_element
      @@ Markup.DNS.make_item x in
    Element.append_child dns_list#root item;
    _ripples <- (item, Ripple.attach item) :: _ripples;
    dns_list#layout ()

  method private remove_item (item : Dom_html.element Js.t) : unit =
    (match List.find_opt (Element.equal item % fst) _ripples with
     | None -> ()
     | Some (_, r) ->
       _ripples <- List.filter (Element.equal item % fst) _ripples;
       Ripple.destroy r);
    Element.remove_child_safe dns_list#root item

  method private handle_item_action e _ : unit Lwt.t =
    let detail = Widget.event_detail e in
    let original_target = Dom.eventTarget detail##.originalEvent in
    if Element.has_class original_target Item_list.CSS.item_meta
    then self#remove_item detail##.item;
    Lwt.return_unit

end

let make (init : Network_config.ipv4_conf) : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.DNS.make init in
  new t elt
