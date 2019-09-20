open Components_tyxml

module CSS = struct
  let root = Util.CSS.root ^ "-service-info"

  let description = BEM.add_modifier root "description"
end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Divider_markup = Divider.Make (Xml) (Svg) (Html)
  module Service_general_info_markup = Service_general_info.Make (Xml) (Svg) (Html)
  module Service_sdt_info_markup = Service_sdt_info.Make (Xml) (Svg) (Html)
  module Pid_overview_markup = Pid_overview.Make (Xml) (Svg) (Html)
  module Tab_markup = Tab.Make (Xml) (Svg) (Html)
  module Tab_bar_markup = Tab_bar.Make (Xml) (Svg) (Html)
  module Glide_markup = Components_lab_tyxml.Glide.Make (Xml) (Svg) (Html)

  let create_description ?(classes = []) ?(a = []) () =
    let classes = CSS.description :: classes in
    div ~a:(a_class classes :: a) []

  let create_tab_bar ?classes ?a ?tabs () =
    let tabs =
      match tabs with
      | Some x -> x
      | None ->
          Tab_markup.
            [ tab ~text_label:(`Text "Общее описание") ()
            ; tab ~text_label:(`Text "SDT") ()
            ; tab ~text_label:(`Text "PIDs") () ]
    in
    Tab_bar_markup.tab_bar ?classes ?a ~tabs ()

  let create_glide
      ?classes
      ?a
      ?children
      ?pids
      ?info
      ?bitrate
      ?max_bitrate
      ?min_bitrate
      ~control
      () =
    let slides =
      match children with
      | Some x -> x
      | None ->
          Glide_markup.
            [ glide_slide
                ~children:
                  [ Service_general_info_markup.create
                      ?info
                      ?bitrate
                      ?max_bitrate
                      ?min_bitrate
                      () ]
                ()
            ; glide_slide ~children:[Service_sdt_info_markup.create ?info ()] ()
            ; glide_slide
                ~children:[Pid_overview_markup.create ?init:pids ~control ()]
                () ]
    in
    Glide_markup.glide ?classes ?a ~slides ()

  let create
      ?(classes = [])
      ?(a = [])
      ?pids
      ?info
      ?bitrate
      ?max_bitrate
      ?min_bitrate
      ?children
      ~control
      () =
    let classes = CSS.root :: classes in
    let children =
      match children with
      | Some x -> x
      | None ->
          [ create_tab_bar ()
          ; Divider_markup.divider_hr ()
          ; create_glide ?pids ?info ?bitrate ?max_bitrate ?min_bitrate ~control () ]
    in
    div ~a:(a_class classes :: a) children
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
