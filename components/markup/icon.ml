open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  type size = [ `S18 | `S24 | `S32 | `S48 ]

  let base_class     = "mdc-icon"
  let button_class   = CSS.add_modifier base_class "button"
  let disabled_class = CSS.add_modifier base_class "disabled"

  module Font = struct
    let create ?(classes=[]) ?attrs ~icon () =
      Html.i ~a:([a_class ("material-icons" :: base_class :: classes)] <@> attrs) [pcdata icon]
  end

  module SVG = struct

    module Path = struct

      let tv =
        "M21,17H3V5H21M21,3H3A2,2 0 0,0 1,5V17A2,2 0 0,0 3,
         19H8V21H16V19H21A2,2 0 0,0 23,17V5A2,2 0 0,0 21,3Z"

      let chevron_down =
        "M7.41,8.58L12,13.17L16.59,8.58L18,10L12,16L6,10L7.41,8.58Z"

      let chevron_up =
        "M7.41,15.41L12,10.83L16.59,15.41L18,14L12,8L6,14L7.41,15.41Z"

      let lock =
        "M12,17A2,2 0 0,0 14,15C14,13.89 13.1,13 12,13A2,2 0 0,0 10,15A2,
         2 0 0,0 12,17M18,8A2,2 0 0,1 20,10V20A2,2 0 0,1 18,22H6A2,2 0 0,
         1 4,20V10C4,8.89 4.9,8 6,8H7V6A5,5 0 0,1 12,1A5,5 0 0,1 17,6V8H18M12,
         3A3,3 0 0,0 9,6V8H15V6A3,3 0 0,0 12,3Z"

      let clock_outline =
        "M12,20A8,8 0 0,0 20,12A8,8 0 0,0 12,4A8,8 0 0,0 4,12A8,8 0 0,0 12,
         20M12,2A10,10 0 0,1 22,12A10,10 0 0,1 12,22C6.47,22 2,17.5 2,12A10,
         10 0 0,1 12,2M12.5,7V12.25L17,14.92L16.25,16.15L11,13V7H12.5Z"

      let download =
        "M5,20H19V18H5M19,9H15V3H9V9H5L12,16L19,9Z"

      type t =
        | Tv | Chevron_down | Chevron_up | Lock | Clock_outline | Download

      let to_string = function
        | Tv            -> tv
        | Chevron_down  -> chevron_down
        | Chevron_up    -> chevron_up
        | Lock          -> lock
        | Clock_outline -> clock_outline
        | Download      -> download

    end

    let create_path ?(classes=[]) ?attrs ?fill d () =
      Svg.path ~a:([ Svg.a_class classes
                   ; Svg.a_d (Path.to_string d) ]
                   |> map_cons_option (fun x -> Svg.a_fill @@ `Color (x, None))
                        fill
                   <@> attrs) []

    let create ?(classes=[]) ?attrs ?(size=24) paths () =
      let sz    = Printf.sprintf "width:%dpx;height:%dpx" size size in
      let sz_fl = float_of_int size in
      svg ~a:([ Svg.a_class (base_class :: classes)
              ; Svg.a_style sz
              ; Svg.a_viewBox (0.,0.,sz_fl,sz_fl)
              ] <@> attrs)
        paths

  end

end
