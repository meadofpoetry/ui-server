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
  module Service_general_info_markup =
    Service_general_info.Make (Xml) (Svg) (Html)
  module Service_sdt_info_markup = Service_sdt_info.Make (Xml) (Svg) (Html)
  module Pid_overview_markup = Pid_overview.Make (Xml) (Svg) (Html)
  module Tab_markup = Tab.Make (Xml) (Svg) (Html)
  module Tab_bar_markup = Tab_bar.Make (Xml) (Svg) (Html)
  module Glide_markup = Components_lab_tyxml.Glide.Make (Xml) (Svg) (Html)

  let ( @:: ) x l = cons (return x) l

  let get_service_bitrate service bitrate =
    Xml.Wutils.l2
      (fun selected rate ->
        match (selected, rate) with
        | None, _ | _, None -> None
        | Some (id, _), Some (rate : Board_niitv_tsan_types.Bitrate.ext) ->
            List.assoc_opt id rate.services)
      service bitrate

  let get_service_pids service pids =
    Xml.Wutils.l2
      (fun service pids ->
        match service with
        | None -> []
        | Some (_, (info : Board_niitv_tsan_types.Service.t)) ->
            List.filter (fun (pid, _) -> List.mem pid info.elements) pids)
      service pids

  (** Merges total bitrate with service bitrate. Needed to show percentarge from
      service bitrate for internal pids overview widget. *)
  let merge_bitrate service_rate bitrate =
    Xml.Wutils.l2
      (fun service rate ->
        match (service, rate) with
        | None, _ | _, None -> None
        | Some v, Some (rate : Board_niitv_tsan_types.Bitrate.ext) ->
            Some { rate with total = v; effective = v })
      service_rate bitrate

  let create_description ?(a = []) () =
    let classes = return [ CSS.description ] in
    div ~a:(a_class classes :: a) (nil ())

  let create_tab_bar ?a () =
    let tabs =
      Tab_markup.tab ~text_label:(return "Общее описание") ()
      @:: Tab_markup.tab ~text_label:(return "SDT") ()
      @:: Tab_markup.tab ~text_label:(return "PIDs") ()
      @:: nil ()
    in
    Tab_bar_markup.tab_bar ?a ~tabs ()

  let create_glide ?a ?pids ?hex ?info ?bitrate ~control () =
    let service_bitrate =
      match (info, bitrate) with
      | None, _ | _, None -> None
      | Some service, Some bitrate -> Some (get_service_bitrate service bitrate)
    in
    let bitrate =
      match (service_bitrate, bitrate) with
      | None, _ | _, None -> None
      | Some service_rate, Some bitrate ->
          Some (merge_bitrate service_rate bitrate)
    in
    let service_pids =
      match (info, pids) with
      | None, _ | _, None -> None
      | Some service, Some pids -> Some (get_service_pids service pids)
    in
    let general_info =
      Service_general_info_markup.create ?hex ?info ?bitrate:service_bitrate ()
    in
    let sdt_info = Service_sdt_info_markup.create ?info () in
    let pid_overview =
      Pid_overview_markup.create ?hex
        ?init:(Option.map Xml.Wutils.totlist service_pids)
        ?bitrate ~control ()
    in
    let slides =
      Glide_markup.glide_slide ~children:(singleton (return general_info)) ()
      @:: Glide_markup.glide_slide ~children:(singleton (return sdt_info)) ()
      @:: Glide_markup.glide_slide
            ~children:(singleton (return pid_overview))
            ()
      @:: nil ()
    in
    Glide_markup.glide ?a ~slides ()

  (** Create service info widget. *)
  let create ?(classes = return []) ?(a = []) ?hex ?pids ?info ?bitrate ~control
      () =
    let classes = fmap (fun x -> CSS.root :: x) classes in
    let children =
      create_tab_bar ()
      @:: Divider_markup.divider_hr ()
      @:: create_glide ?hex ?pids ?info ?bitrate ~control ()
      @:: nil ()
    in
    div ~a:(a_class classes :: a) children
end

module F = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
