open Components_tyxml

let id = "tz-config"

let tz_id = "tz-selector"

module CSS = struct
  let root = "stream-select"

  let label = BEM.add_element root "label"

  let select = BEM.add_element root "select"
end

module Make
         (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  open Xml.W

  open Select.Make (Xml) (Svg) (Html)

  open Form_field.Make (Xml) (Svg) (Html)

  open Ui_templates_tyxml.Settings_page.Make (Xml) (Svg) (Html)
  
  let create ?classes ?(a = []) (options : string list)
        (v : Pc_control_types.Timedate_config.t) =
    let tz_selector =
      let tz_input_id = tz_id ^ "-id" in
      let classes = Option.map (fmap (fun x -> CSS.select :: x)) classes in
      let options = List.map (fun opt ->
                        if opt == v.timezone
                        then Native.option ?classes ~text:opt ~selected:true ()
                        else Native.option ?classes ~text:opt ())
                      options
      in
      Native.select ~a:[ a_id tz_id ] ?classes ~outlined:true
        ~input_id:tz_input_id
        ~label:(return "Временные зоны") ~options ()
    in
    let header =
      create_section_header ~title:(`Text "Временная зона") ()
    in
    create_section
      ?classes
      ~a:(a_id id::a)
      ~header
      ~children:[ tz_selector ]
      ()
    
end
