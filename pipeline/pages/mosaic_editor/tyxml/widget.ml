open Components_tyxml
open Components_tyxml.Utils
open Pipeline_types

let widget_type_to_string : Wm.widget_type -> string = function
  | Video -> "video"
  | Audio -> "audio"

let widget_type_of_string : string -> Wm.widget_type option = function
  | "video" -> Some Video
  | "audio" -> Some Audio
  | _ -> None

let widget_type_to_svg_path : Wm.widget_type -> string = function
  | Video -> Svg_icons.video
  | Audio -> Svg_icons.music

let domain_attr_value (domain : Wm.domain) =
  Yojson.Safe.to_string @@ Wm.domain_to_yojson domain

let aspect_attr_value (w, h : int * int) =
  Printf.sprintf "%dx%d" w h

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct

  let to_html_attributes ?id (widget : Wm.widget) =
    let aspect = match widget.aspect with
      | None -> None
      | Some ar -> Some (aspect_attr_value ar) in
    Html.(
      [ a_user_data "type" (widget_type_to_string widget.type_)
      ; a_user_data "domain" (domain_attr_value widget.domain)
      ; a_user_data "description" widget.description ]
      |> map_cons_option a_id id
      |> map_cons_option (a_user_data "aspect") aspect
      |> map_cons_option (a_user_data "pid" % string_of_int) widget.pid)

end
