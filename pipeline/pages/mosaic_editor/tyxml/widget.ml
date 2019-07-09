open Components_tyxml
open Components_tyxml.Utils
open Pipeline_types

let widget_type_to_string : Wm.widget_type -> string = function
  | Video -> "video"
  | Audio -> "audio"

let widget_type_to_svg_path : Wm.widget_type -> string = function
  | Video -> Svg_icons.video
  | Audio -> Svg_icons.music

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct

  let to_html_attributes ?id (widget : Wm.widget) =
    let domain = Yojson.Safe.to_string @@ Wm.domain_to_yojson widget.domain in
    let aspect = match widget.aspect with
      | None -> None
      | Some (w, h) -> Some (Printf.sprintf "%dx%d" w h) in
    Html.(
      [ a_user_data "type" (widget_type_to_string widget.type_)
      ; a_user_data "domain" domain
      ; a_user_data "description" widget.description ]
      |> map_cons_option a_id id
      |> map_cons_option (a_user_data "aspect") aspect
      |> map_cons_option (a_user_data "pid" % string_of_int) widget.pid)

end
