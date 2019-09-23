open Components_tyxml

module CSS = struct
  let root = Util.CSS.root ^ "-service-info"

  let description = BEM.add_modifier root "description"
end

module Make
    (Xml : Intf.Xml)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html
  module Divider_markup = Divider.Make (Xml) (Svg) (Html)
  module Service_general_info_markup = Service_general_info.Make (Xml) (Svg) (Html)
  module Service_sdt_info_markup = Service_sdt_info.Make (Xml) (Svg) (Html)
  module Pid_overview_markup = Pid_overview.Make (Xml) (Svg) (Html)
  module Tab_markup = Tab.Make (Xml) (Svg) (Html)
  module Tab_bar_markup = Tab_bar.Make (Xml) (Svg) (Html)
  module Glide_markup = Components_lab_tyxml.Glide.Make (Xml) (Svg) (Html)

  let ( @:: ) x l = cons (return x) l

  let create_description ?(a = []) () =
    let classes = return [CSS.description] in
    div ~a:(a_class classes :: a) (nil ())

  let create_tab_bar ?a () =
    let tabs =
      Tab_markup.tab ~text_label:(return "Общее описание") ()
      @:: Tab_markup.tab ~text_label:(return "SDT") ()
      @:: Tab_markup.tab ~text_label:(return "PIDs") ()
      @:: nil ()
    in
    Tab_bar_markup.tab_bar ?a ~tabs ()

  let create_glide ?a ?pids ?info ?bitrate ?max_bitrate ?min_bitrate ~control () =
    let general_info =
      Service_general_info_markup.create ?info ?bitrate ?max_bitrate ?min_bitrate ()
    in
    let sdt_info = Service_sdt_info_markup.create ?info () in
    let pid_overview = Pid_overview_markup.create ?init:pids ~control () in
    let slides =
      Glide_markup.glide_slide ~children:(singleton (return general_info)) ()
      @:: Glide_markup.glide_slide ~children:(singleton (return sdt_info)) ()
      @:: Glide_markup.glide_slide ~children:(singleton (return pid_overview)) ()
      @:: nil ()
    in
    Glide_markup.glide ?a ~slides ()

  let create ?(a = []) ?pids ?info ?bitrate ?max_bitrate ?min_bitrate ~control () =
    let children =
      create_tab_bar ()
      @:: Divider_markup.divider_hr ()
      @:: create_glide ?pids ?info ?bitrate ?max_bitrate ?min_bitrate ~control ()
      @:: nil ()
    in
    div ~a:(a_class (return [CSS.root]) :: a) children
end

module F = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
