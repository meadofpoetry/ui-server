open Tyxml
open Application_types

let ( % ) f g x = f (g x)

module Markup = Page_input.Make(Xml)(Svg)(Html)

module Api_template = Api_cohttp_template.Make(User)

module type S = sig
  val stylesheets : string list
  val pre_scripts : Api_template.script list
  val post_scripts : Api_template.script list
  val tabs : (string * string * Tyxml.Xml.elt list) list
end

let common_tabs () = []

let tabs_of_cpu = function
  | None ->
    []
  | Some _ ->
    [(module Pipeline_page_input : S)]

let tabs_of_board ({ model; manufacturer; _ } as b : Topology.topo_board) =
  let module M = struct let topo_board = b end in
  match manufacturer, model with
  | "DekTec", "DTM-3200" ->
    [(module Board_dektec_dtm3200_page_input.Make(M) : S)]
  | "NIITV", "DVB4CH" ->
    [(module Board_niitv_dvb_page_input.Make(M) : S)]
  | "NIITV", "TSAN" ->
    [(module Board_niitv_tsan_page_input.Make(M))]
  | _ -> []

let make_template
    (input : Topology.topo_input)
    (cpu : Topology.topo_cpu option)
    (boards : Topology.topo_board list) =
  let tabs_content =
    common_tabs ()
    @ (List.flatten @@ List.map tabs_of_board boards)
    @ tabs_of_cpu cpu in
  let title = Topology.get_input_name input in
  let tabs, stylesheets, pre_scripts, post_scripts =
    List.fold_left (fun (tabs, stylesheets, pre_scripts, post_scripts) m ->
        let (module M : S) = m in
        ( tabs @ M.tabs
        , stylesheets @ M.stylesheets
        , pre_scripts @ M.pre_scripts
        , post_scripts @ M.post_scripts))
      ( []
      , ["/css/page-input.min.css"]
      , []
      , [`Src "/js/page-input.js"] )
      tabs_content in
  let tab_bar =
    Markup.make_tab_bar
    @@ List.map (fun (id, label, _) -> Markup.make_tab ~id label) tabs in
  let slides =
    Markup.make_content
    @@ List.map (fun (id, _, c) -> Markup.make_tabpanel ~id @@ Html.totl c) tabs in
  Api_template.make_template_props
    ~title
    ~stylesheets
    ~pre_scripts
    ~post_scripts
    ~top_app_bar_bottom:(Tyxml.Html.toelt tab_bar)
    ~content:[Tyxml.Html.toelt slides]
    ()
