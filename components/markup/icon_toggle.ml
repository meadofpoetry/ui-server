open Utils

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct

  open Html

  type data =
    { icon      : string        [@key "content"]
    ; label     : string option
    ; css_class : string option [@key "cssClass"]
    } [@@deriving to_yojson]

  let base_class     = "mdc-icon-toggle"
  let icons_class    = "material-icons"
  let disabled_class = CSS.add_modifier base_class "disabled"

  let create ?(classes=[]) ?attrs ?(disabled=false) ?color_scheme ~on_data ~off_data () =
    let data_toggle_on  = on_data |> data_to_yojson |> Yojson.Safe.to_string in
    let data_toggle_off = off_data |> data_to_yojson |> Yojson.Safe.to_string in
    i ~a:([ a_class (classes
                     |> cons_if disabled @@ CSS.add_modifier base_class "disabled"
                     |> map_cons_option (function
                            | `Primary -> CSS.add_modifier base_class "primary"
                            | `Accent  -> CSS.add_modifier base_class "accent")
                          color_scheme
                     |> List.cons base_class
                     |> List.cons icons_class)
          ; a_role ["button"]
          ; a_user_data "toggle-on"  data_toggle_on
          ; a_user_data "toggle-off" data_toggle_off
          ; a_aria "pressed" ["false"]
          ; a_tabindex (if disabled then -1 else 0) ]
          |> map_cons_option (fun x -> a_aria "label" [x]) off_data.label
          |> cons_if disabled @@ a_aria "disabled" ["true"]
          <@> attrs)
      [pcdata off_data.icon]

end
