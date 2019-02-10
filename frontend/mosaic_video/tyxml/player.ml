open Components_tyxml
open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  module CSS = struct

    let root = "mdc-video-player"
    let video = CSS.add_element root "video"
    let gradient = root ^ "-controls-gradient"

    let theater = CSS.add_modifier root "theater"

    module Controls = struct
      let root = root ^ "-controls"
      let action = CSS.add_element root "action"
      let section = CSS.add_element root "section"

      let section_start = CSS.add_modifier section "align-start"
      let section_end = CSS.add_modifier section "align-end"
      let action_play = CSS.add_modifier action "play"
      let action_fullscreen = CSS.add_modifier action "fullscreen"
    end

  end

  module Controls = struct

    module Icon_button = Icon_button.Make(Xml)(Svg)(Html)

    type align = [`Start | `End]

    let create_action ?(classes = []) ?attrs ?on_icon ~icon () : 'a elt =
      let classes = CSS.Controls.action :: classes in
      Icon_button.create ?attrs ?on_icon ~classes ~icon ()

    let create_section ?(classes = []) ?attrs ?(align : align option)
          content () : 'a elt =
      let classes =
        classes
        |> map_cons_option (function
               | `End -> CSS.Controls.section_end
               | `Start -> CSS.Controls.section_start) align
        |> List.cons CSS.Controls.section in
      div ~a:([a_class classes] <@> attrs) content

    let create ?(classes = []) ?attrs sections () : 'a elt =
      let classes = CSS.Controls.root :: classes in
      div ~a:([a_class classes] <@> attrs) sections

  end

  let create_video ?(classes = []) ?attrs
        ?(autoplay = false)
        ?(playsinline = false)
        ?(controls = true)
        () : 'a elt =
    let classes = CSS.video :: classes in
    video ~a:([a_class classes]
              |> cons_if_lazy controls a_controls
              |> cons_if_lazy autoplay a_autoplay
              |> cons_if_lazy playsinline (fun () ->
                     Unsafe.string_attrib "playsinline" "true")
              <@> attrs) []

  let create_gradient ?(classes = []) ?attrs () : 'a elt =
    let classes = CSS.gradient :: classes in
    div ~a:([a_class classes] <@> attrs) []

  let create ?(classes = []) ?attrs ?(theater_mode = false)
        ?controls ?gradient ~video () : 'a elt =
    let classes =
      classes
      |> cons_if theater_mode CSS.theater
      |> List.cons CSS.root in
    div ~a:([a_class classes] <@> attrs)
      (video :: (gradient ^:: controls ^:: []))

end
