open Components_tyxml

module CSS = struct
  let root = "player"

  (* Element playing audio from the selected service *)
  let audio = BEM.add_element root "audio"

  (* Element playing video and alarm audio *)
  let video = BEM.add_element root "video"

  let gradient = root ^ "-controls-gradient"

  let overlay = BEM.add_element root "overlay"

  let state_overlay = BEM.add_element root "state-overlay"

  let state_overlay_icon = BEM.add_element root "state-overlay-icon"

  let state_overlay_wrapper = BEM.add_element root "state-overlay-wrapper"

  let big_button = BEM.add_element root "big-button"

  let autohide = BEM.add_modifier root "autohide"

  let paused = BEM.add_modifier root "paused"

  let playing = BEM.add_modifier root "playing"

  let big_mode = BEM.add_modifier root "big-mode"

  module Controls = struct
    let root = root ^ "-controls"

    let action = BEM.add_element root "action"

    let section = BEM.add_element root "section"

    let section_start = BEM.add_modifier section "align-start"

    let section_end = BEM.add_modifier section "align-end"

    let action_play = BEM.add_modifier action "play"

    let action_mute = BEM.add_modifier action "mute"

    let action_fullscreen = BEM.add_modifier action "fullscreen"

    let volume_panel = BEM.add_element root "volume-panel"

    let volume = BEM.add_element root "volume"
  end
end

module Path = struct
  open Svg_icons

  let video = video

  let play = play

  let pause = pause

  let fullscreen = fullscreen

  let fullscreen_exit = fullscreen_exit

  let volume_off = volume_off

  let volume_low = volume_low

  let volume_medium = volume_medium

  let volume_high = volume_high
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Icon = Icon.Make (Xml) (Svg) (Html)
  module Icon_button = Icon_button.Make (Xml) (Svg) (Html)

  module Controls = struct
    type align =
      [ `Start
      | `End ]

    let create_action ?(classes = []) ?a ?ripple ?disabled ?on_icon ~icon () : 'a elt =
      let classes = CSS.Controls.action :: classes in
      Icon_button.icon_button ?a ?ripple ?disabled ?on_icon ~classes ~icon ()

    let create_volume_panel ?(classes = []) ?(attrs = []) content () : 'a elt =
      let classes = CSS.Controls.volume_panel :: classes in
      div ~a:([a_class classes] @ attrs) content

    let create_section ?(classes = []) ?(attrs = []) ?(align : align option) content () :
        'a elt =
      let classes =
        classes
        |> Utils.map_cons_option
             (function
               | `End -> CSS.Controls.section_end
               | `Start -> CSS.Controls.section_start)
             align
        |> List.cons CSS.Controls.section
      in
      div ~a:([a_class classes] @ attrs) content

    let create ?(classes = []) ?(attrs = []) sections () : 'a elt =
      let classes = CSS.Controls.root :: classes in
      div ~a:([a_class classes] @ attrs) sections
  end

  let create_audio
      ?(classes = [])
      ?(attrs = [])
      ?(controls = true)
      ?(autoplay = false)
      ?(playsinline = false)
      () : 'a elt =
    let classes = CSS.audio :: classes in
    audio
      ~a:
        ([a_class classes; Unsafe.string_attrib "preload" "auto"] @ attrs
        |> Utils.cons_if_lazy controls a_controls
        |> Utils.cons_if_lazy autoplay a_autoplay
        |> Utils.cons_if_lazy playsinline (fun () ->
               Unsafe.string_attrib "playsinline" "true"))
      []

  let create_video
      ?(classes = [])
      ?(attrs = [])
      ?(autoplay = false)
      ?(playsinline = false)
      ?(controls = true)
      () : 'a elt =
    let classes = CSS.video :: classes in
    video
      ~a:
        ([a_class classes; Unsafe.string_attrib "preload" "auto"] @ attrs
        |> Utils.cons_if_lazy controls a_controls
        |> Utils.cons_if_lazy autoplay a_autoplay
        |> Utils.cons_if_lazy playsinline (fun () ->
               Unsafe.string_attrib "playsinline" "true"))
      []

  let create_state_overlay ?(classes = []) ?(attrs = []) path () : 'a elt =
    let classes = CSS.state_overlay_wrapper :: classes in
    div
      ~a:([a_class classes; a_style "display: none;"] @ attrs)
      [ div
          ~a:[a_class [CSS.state_overlay]]
          [Icon.SVG.icon ~classes:[CSS.state_overlay_icon] ~d:path ()] ]

  let create_gradient ?(classes = []) ?(attrs = []) () : 'a elt =
    let classes = CSS.gradient :: classes in
    div ~a:([a_class classes] @ attrs) []

  let create
      ?(classes = [])
      ?(attrs = [])
      ?controls
      ?gradient
      ?state_overlay
      ?audio
      ~video
      () : 'a elt =
    let classes = CSS.root :: classes in
    div
      ~a:([a_class classes; a_tabindex (-1)] @ attrs)
      Utils.(video :: (audio ^:: state_overlay ^:: gradient ^:: controls ^:: []))
end
