open Containers
open Tyxml_js

type action =
  [ `Accept
  | `Cancel
  ]

let compare_action a b =
  match a, b with
  | `Accept, `Accept | `Cancel, `Cancel -> 0
  | `Accept, _ -> 1
  | `Cancel, _ -> -1

module Markup = Components_markup.Dialog.Make(Xml)(Svg)(Html)

let animation_time_ms = 120.

module Action = struct

  type t =
    { button : Widget.t
    ; typ : action
    }

  let make ~typ (button : #Button.t) =
    button#add_class Markup.Footer.button_class;
    button#add_class (match typ with
                      | `Accept -> Markup.Footer.accept_button_class
                      | `Cancel -> Markup.Footer.cancel_button_class);
    { button = button#widget
    ; typ
    }

end

module Header = struct

  class t ~title () =
    let elt = Markup.Header.create ~title () |> To_dom.of_header in
    object

      val h2_widget =
        elt##querySelector (Js.string @@ "." ^ Markup.Header.title_class)
        |> Js.Opt.to_option |> Option.get_exn |> Widget.create

      inherit Widget.t elt ()

      method title : string =
        h2_widget#text_content |> Option.get_or ~default:""

      method set_title (s : string) : unit =
        h2_widget#set_text_content s
    end

end

module Body = struct

  class t ?scrollable
          ~(content : [ `String of string | `Widgets of #Widget.t list ]) () =
    let content = match content with
      | `String s -> [ Html.pcdata s ]
      | `Widgets w -> List.map Widget.to_markup w in
    let elt = Markup.Body.create ?scrollable ~content () |> To_dom.of_element in
    object
      inherit Widget.t elt () as super

      method set_scrollable (x : bool) : unit =
        super#add_or_remove_class x Markup.Body.scrollable_class
    end

end

module Footer = struct

  class t ?(sort_actions = true) ~(actions : Action.t list) () =
    let actions =
      if not sort_actions then actions else
        List.sort (fun (a : Action.t) b ->
            compare_action a.typ b.typ) actions in
    let elt =
      Markup.Footer.create
        ~children:(List.map (fun (x : Action.t) ->
                       Widget.to_markup x.button) actions) ()
      |> To_dom.of_footer in
    object

      val mutable _actions = actions
      inherit Widget.t elt () as super

      method destroy () : unit =
        super#destroy ();
        List.iter (fun (x : Action.t) ->
            x.button#destroy ()) _actions

      method actions = _actions

    end

end

class t ?scrollable
        ?title
        ?sort_actions
        ?(actions : Action.t list option)
        ~content () =
  let header_widget =
    Option.map (fun x -> new Header.t ~title:x ()) title in
  let body_widget =
    new Body.t ?scrollable ~content () in
  let footer_widget =
    Option.map (fun x -> new Footer.t ?sort_actions ~actions:x ())
      actions in

  let content =
    List.empty
    |> List.cons_maybe @@ Option.map Widget.to_markup footer_widget
    |> List.cons @@ Widget.to_markup body_widget
    |> List.cons_maybe @@ Option.map Widget.to_markup header_widget in
  let surface =
    Markup.create_surface content ()
    |> To_dom.of_div
    |> Widget.create in
  let backdrop =
    Markup.create_backdrop ()
    |> To_dom.of_div
    |> Widget.create in
  let elt =
    Markup.create
      ~surface:(Widget.to_markup surface)
      ~backdrop:(Widget.to_markup backdrop) ()
    |> To_dom.of_aside in
  let e_action, set_action = React.E.create () in

  object(self)

    val mutable _timer = None
    val mutable _opened = false
    val mutable _keydown = None
    val mutable _bd_click = None
    val mutable _action_listeners = []

    inherit Widget.t elt () as super

    method init () : unit =
      List.iter (fun (a : Action.t) ->
          match a.typ with
          | `Accept ->
             let l =
               a.button#listen_click_lwt (fun _ _ ->
                   self#_accept (); Lwt.return_unit) in
             _action_listeners <- l :: _action_listeners;
          | `Cancel ->
             let l =
               a.button#listen_click_lwt (fun _ _ ->
                   self#_cancel (); Lwt.return_unit) in
             _action_listeners <- l :: _action_listeners)
      @@ Option.get_or ~default:[] actions

    method destroy () : unit =
      super#destroy ();
      Option.iter (fun x -> x#destroy ()) header_widget;
      body_widget#destroy ();
      Option.iter (fun x -> x#destroy ()) footer_widget;
      if self#opened then self#hide ();
      self#remove_class Markup.animating_class;
      self#_clear_timer ();
      List.iter Lwt.cancel _action_listeners;
      _action_listeners <- [];
      Option.iter Lwt.cancel _bd_click;
      _bd_click <- None;
      Option.iter Dom_events.stop_listen _keydown;
      _keydown <- None

    method header = header_widget
    method body = body_widget
    method footer = footer_widget

    method opened : bool =
      _opened

    method show () : unit =
      _opened <- true;
      self#_disable_scroll true;
      (* Listen backdrop click *)
      backdrop#listen_click_lwt (fun _ _ ->
          self#_cancel (); Lwt.return_unit)
      |> (fun x -> _bd_click <- Some x);
      (* Listen escape key *)
      Dom_events.listen Dom_html.document Widget.Event.keydown (fun _ e ->
          match Utils.Keyboard_event.event_to_key e with
          | `Escape -> self#_cancel (); false
          | _ -> true)
      |> (fun x -> _keydown <- Some x);
      self#_clear_timer ();
      self#_set_timer ();
      self#add_class Markup.animating_class;
      self#add_class Markup.open_class

    method show_await () : action Lwt.t =
      self#show ();
      Lwt_react.E.next e_action

    method hide () =
      _opened <- false;
      self#_disable_scroll false;
      Option.iter Dom_events.stop_listen _keydown;
      Option.iter Lwt.cancel _bd_click;
      (* TODO add untrap focus *)
      self#_clear_timer ();
      self#_set_timer ();
      self#add_class Markup.animating_class;
      self#remove_class Markup.open_class

    method e_action : action React.event = e_action

    (* Private methods *)

    method private _accept () =
      set_action `Accept;
      self#hide ()

    method private _cancel () =
      set_action `Cancel;
      self#hide ()

    method private _disable_scroll (x : bool) : unit =
      let _class = Js.string Markup.scroll_lock_class in
      let class_list = Dom_html.document##.body##.classList in
      if x then class_list##add _class
      else class_list##remove _class

    method private _clear_timer () =
      begin match _timer with
      | None -> ()
      | Some t -> Dom_html.clearTimeout t
      end;
      _timer <- None

    method private _set_timer () =
      (* TODO add focus on accept if animation ended and dialog is opened *)
      let f = fun () -> self#remove_class Markup.animating_class in
      Dom_html.setTimeout f animation_time_ms
      |> fun x -> _timer <- Some x

  end
