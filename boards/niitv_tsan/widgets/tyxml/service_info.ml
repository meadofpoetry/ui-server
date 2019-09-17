open Components_tyxml

module CSS = struct
  let root = Util.CSS.root ^ "-service-info"

  let description = BEM.add_modifier root "description"
end

module Make
    (Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml and module Svg := Svg) =
struct
  open Html
  module Divider_markup = Divider.Make (Xml) (Svg) (Html)
  module Service_general_info_markup = Service_general_info.Make (Xml) (Svg) (Html)
  module Service_sdt_info_markup = Service_sdt_info.Make (Xml) (Svg) (Html)
  module Pid_overview_markup = Pid_overview.Make (Xml) (Svg) (Html)
  module Tab_markup = Tab.Make (Xml) (Svg) (Html)
  module Tab_bar_markup = Tab_bar.Make (Xml) (Svg) (Html)
  module Glide_markup = Components_lab_tyxml.Glide.Make (Xml) (Svg) (Html)

  let create_description ?(classes = []) ?(attrs = []) () =
    let classes = CSS.description :: classes in
    div ~a:([a_class classes] @ attrs) []

  let create_tab_bar ?classes ?attrs ?tabs () =
    let tabs =
      match tabs with
      | Some x -> x
      | None ->
          Tab_markup.
            [ create ~text_label:(`Text "Общее описание") ()
            ; create ~text_label:(`Text "SDT") ()
            ; create ~text_label:(`Text "PIDs") () ]
    in
    Tab_bar_markup.create ?classes ?attrs ~tabs ()

  let create_glide
      ?classes
      ?attrs
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
            [ create_slide
                ~children:
                  [ Service_general_info_markup.create
                      ?info
                      ?bitrate
                      ?max_bitrate
                      ?min_bitrate
                      () ]
                ()
            ; create_slide ~children:[Service_sdt_info_markup.create ?info ()] ()
            ; create_slide
                ~children:[Pid_overview_markup.create ?init:pids ~control ()]
                () ]
    in
    Glide_markup.create ?classes ?attrs ~slides ()

  let create
      ?(classes = [])
      ?(attrs = [])
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
          ; Divider_markup.create_hr ()
          ; create_glide ?pids ?info ?bitrate ?max_bitrate ?min_bitrate ~control () ]
    in
    div ~a:([a_class classes] @ attrs) children
end

module Markup = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
