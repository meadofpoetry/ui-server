(*
 * TODO
 * add keyboard focus + styling
 * add enable/disable + styling
 *)
open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_markup.Expansion_panel.Make(Xml)(Svg)(Html)

module Primary = struct
  class t ~(title : string)
          ~(heading_details : #Widget.t list)
          ~(details : #Widget.t list)
          () =
    let elt = Markup.Primary.create ~title
                ~heading_details:(List.map Widget.to_markup heading_details)
                ~details:(List.map Widget.to_markup details) ()
              |> To_dom.of_element in
    object
      inherit Widget.t elt ()
    end
end

module Panel = struct
  class t ~(content : #Widget.t list) () =
    let content = List.map Widget.to_markup content in
    let elt = To_dom.of_element @@ Markup.Panel.create ~content () in
    object
      inherit Widget.t elt ()
    end
end

module Actions = struct
  class t ~(actions : #Widget.t list) () =
    let actions = List.map Widget.to_markup actions in
    let elt = To_dom.of_element @@ Markup.Actions.create ~actions () in
    object
      inherit Widget.t elt ()
    end
end

class t ?(expanded = false)
        ?(elevation = 2)
        ?(actions : #Widget.t list option)
        ?(heading_details : #Widget.t list option)
        ?(details : #Widget.t list option)
        ~(title : string)
        ~(content : #Widget.t list)
        () =
  let heading_details = Option.get_or ~default:[] heading_details in
  let details = Option.get_or ~default:[] details in
  let primary = new Primary.t ~title ~details ~heading_details () in
  let actions = Option.map (fun actions -> new Actions.t ~actions ()) actions in
  let panel = new Panel.t ~content () in
  let (elt : Dom_html.element Js.t) =
    Markup.create ~primary:(Widget.to_markup primary)
      ~panel:(Widget.to_markup panel)
      ?actions:(Option.map Widget.to_markup actions)
      ()
    |> To_dom.of_element in
  let wrapper = elt##querySelector (Js.string ("." ^ Markup.panel_wrapper_class))
                |> Js.Opt.to_option |> Option.get_exn |> Widget.create in
  let s_expanded,s_expanded_push = React.S.create expanded in

  object(self)

    val mutable elevation = elevation

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      self#set_elevation elevation;
      self#set_expanded expanded;
      (* FIXME keep events *)
      primary#listen_click_lwt (fun _ _ ->
          self#set_expanded (not self#expanded);
          Lwt.return_unit) |> ignore;
      primary#listen_lwt Widget.Event.keydown (fun e _ ->
          match Utils.Keyboard_event.event_to_key e with
          | `Enter ->
             Dom.preventDefault e;
             self#set_expanded (not self#expanded);
             Lwt.return_unit
          | _ -> Lwt.return_unit) |> ignore

    method s_expanded =
      s_expanded

    method expanded = React.S.value s_expanded
    method set_expanded x =
      self#add_or_remove_class x Markup.expanded_class;
      if not x
      then wrapper#style##.display := Js.string "none"
      else wrapper#style##.display := Js.string "";
      s_expanded_push x

    method elevation =
      elevation
    method set_elevation x =
      Elevation.remove_elevation self;
      self#add_class @@ Elevation.Markup.get_elevation_class x

    method title = title
    method details = List.map Widget.coerce details
    method heading_details = List.map Widget.coerce heading_details
    method primary = primary
    method panel = panel
    method actions = actions

  end
