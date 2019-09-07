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

let stream_attr_value = Application_types.Stream.ID.to_string

let channel_attr_value = string_of_int

let aspect_attr_value ((w, h) : int * int) = Printf.sprintf "%dx%d" w h

let stream_of_domain = function
  | Wm.Nihil -> None
  | Chan x -> Some x.stream

let channel_of_domain = function
  | Wm.Nihil -> None
  | Chan x -> Some x.channel

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  let domain_attrs = function
    | Wm.Nihil -> []
    | Chan x ->
        Html.
          [ a_user_data "stream" (stream_attr_value x.stream)
          ; a_user_data "channel" (channel_attr_value x.channel) ]

  let to_html_attributes ?id (widget : Wm.widget) =
    let aspect =
      match widget.aspect with
      | None -> None
      | Some ar -> Some (aspect_attr_value ar)
    in
    let position =
      match widget.position with
      | None -> []
      | Some pos ->
          let string_of_float = Printf.sprintf "%g" in
          Html.
            [ a_user_data "left" (string_of_float pos.x)
            ; a_user_data "top" (string_of_float pos.y)
            ; a_user_data "width" (string_of_float pos.w)
            ; a_user_data "height" (string_of_float pos.h) ]
    in
    Html.(
      [ a_user_data "type" (widget_type_to_string widget.type_)
      ; a_user_data "description" widget.description ]
      @ domain_attrs widget.domain
      @ position
      |> map_cons_option (a_user_data "id") id
      |> map_cons_option (a_user_data "aspect") aspect
      |> map_cons_option (a_user_data "pid" % string_of_int) widget.pid)
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
