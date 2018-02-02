module Primary = struct

  class t ~(title           : string)
          ~(heading_details : #Widget.widget list)
          ~(details         : #Widget.widget list)
          () =
    let elt = Markup.Expansion_panel.Primary.create
                ~title
                ~heading_details:(CCList.map Widget.widget_to_markup heading_details)
                ~details:(CCList.map Widget.widget_to_markup details)
                ()
              |> Tyxml_js.To_dom.of_element
    in
    object
      inherit Widget.widget elt ()
    end

end

module Panel = struct

  class t ~(content : #Widget.widget list) () =
    let elt = Markup.Expansion_panel.Panel.create ~content:(CCList.map Widget.widget_to_markup content) ()
              |> Tyxml_js.To_dom.of_element
    in
    object
      inherit Widget.widget elt ()
    end

end

module Actions = struct

  class t ~(actions : #Widget.widget list) () =
    let elt = Markup.Expansion_panel.Actions.create ~actions:(CCList.map Widget.widget_to_markup actions) ()
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
  let heading_details = CCOpt.get_or ~default:[] heading_details in
  let details = CCOpt.get_or ~default:[] details in
  let primary = new Primary.t ~title ~details ~heading_details () in
  let actions = CCOpt.map (fun actions -> new Actions.t ~actions ()) actions in
  let panel   = new Panel.t ~content () in
  let elt     = Markup.Expansion_panel.create ~primary:(Widget.widget_to_markup primary)
                                              ~panel:(Widget.widget_to_markup panel)
                                              ?actions:(CCOpt.map Widget.widget_to_markup actions)
                                              ()
                |> Tyxml_js.To_dom.of_element
  in
  let wrapper = elt##querySelector (Js.string ("." ^ Markup.Expansion_panel.panel_wrapper_class))
                |> Js.Opt.to_option |> CCOpt.get_exn |> Widget.create in
  let s_expanded,s_expanded_push = React.S.create expanded in

  object(self)

    val mutable elevation = elevation

    inherit Widget.widget elt ()

    method s_expanded     = s_expanded

    method set_expanded x = s_expanded_push x;
                            self#add_or_remove_class (self#get_expanded) Markup.Expansion_panel.expanded_class;
                            if not self#get_expanded then wrapper#style##.display := Js.string "none"
                            else wrapper#style##.display := Js.string ""
    method get_expanded   = React.S.value s_expanded

    method set_elevation x = Elevation.remove_elevation self;
                             self#add_class @@ Elevation.get_elevation_class x
    method get_elevation   = elevation

    initializer
      self#set_elevation elevation;
      self#set_expanded expanded;
      Dom_events.listen primary#root
                        Dom_events.Typ.click
                        (fun _ _ -> self#set_expanded (not self#get_expanded); true)
      |> ignore

  end
