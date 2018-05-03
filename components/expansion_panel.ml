(*
 * TODO
 * add keyboard focus + styling
 * add enable/disable + styling
 *)
open Containers

module Primary = struct

  class t ~(title           : string)
          ~(heading_details : #Widget.widget list)
          ~(details         : #Widget.widget list)
          () =
    let elt = Markup.Expansion_panel.Primary.create
                ~title
                ~heading_details:(List.map Widget.widget_to_markup heading_details)
                ~details:(List.map Widget.widget_to_markup details)
                ()
              |> Tyxml_js.To_dom.of_element
    in
    object
      inherit Widget.widget elt ()
    end

end

module Panel = struct

  class t ~(content : #Widget.widget list) () =
    let elt = Markup.Expansion_panel.Panel.create ~content:(List.map Widget.widget_to_markup content) ()
              |> Tyxml_js.To_dom.of_element
    in
    object
      inherit Widget.widget elt ()
    end

end

module Actions = struct

  class t ~(actions : #Widget.widget list) () =
    let elt = Markup.Expansion_panel.Actions.create ~actions:(List.map Widget.widget_to_markup actions) ()
              |> Tyxml_js.To_dom.of_element
    in
    object
      inherit Widget.widget elt ()
    end

end

class t ?(expanded=false)
        ?(elevation=2)
        ?(actions         : #Widget.widget list option)
        ?(heading_details : #Widget.widget list option)
        ?(details         : #Widget.widget list option)
        ~(title           : string)
        ~(content         : #Widget.widget list)
        () =
  let heading_details = Option.get_or ~default:[] heading_details in
  let details = Option.get_or ~default:[] details in
  let primary = new Primary.t ~title ~details ~heading_details () in
  let actions = Option.map (fun actions -> new Actions.t ~actions ()) actions in
  let panel   = new Panel.t ~content () in
  let elt     = Markup.Expansion_panel.create ~primary:(Widget.widget_to_markup primary)
                                              ~panel:(Widget.widget_to_markup panel)
                                              ?actions:(Option.map Widget.widget_to_markup actions)
                                              ()
                |> Tyxml_js.To_dom.of_element
  in
  let wrapper = elt##querySelector (Js.string ("." ^ Markup.Expansion_panel.panel_wrapper_class))
                |> Js.Opt.to_option |> Option.get_exn |> Widget.create in
  let s_expanded,s_expanded_push = React.S.create expanded in

  object(self)

    val mutable elevation = elevation

    inherit Widget.widget elt ()

    method s_expanded     = s_expanded

    method set_expanded x = self#add_or_remove_class x Markup.Expansion_panel.expanded_class;
                            if not x
                            then wrapper#style##.display := Js.string "none"
                            else wrapper#style##.display := Js.string "";
                            s_expanded_push x
    method expanded   = React.S.value s_expanded


    method elevation       = elevation
    method set_elevation x = Elevation.remove_elevation self;
                             self#add_class @@ Elevation.get_elevation_class x

    method title           = title
    method details         = List.map Widget.coerce details
    method heading_details = List.map Widget.coerce heading_details
    method primary         = primary
    method panel           = panel
    method actions         = actions

    initializer
      self#set_elevation elevation;
      self#set_expanded expanded;
      Dom_events.listen primary#root
                        Dom_events.Typ.click
                        (fun _ _ -> self#set_expanded (not self#expanded); true)
      |> ignore;
      Dom_events.listen primary#root
                        Dom_events.Typ.keydown
                        (fun _ (ev:Dom_html.keyboardEvent Js.t) ->
                          let key  = Option.map Js.to_string @@ Js.Optdef.to_option ev##.key in
                          (match key,ev##.keyCode with
                           | Some "Enter", _ | _, 13 | Some "Space", _ | _, 32 ->
                              Dom.preventDefault ev;
                              self#set_expanded (not self#expanded)
                           | _ -> ());
                          true)
      |> ignore

  end
