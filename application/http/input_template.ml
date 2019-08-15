open Tyxml
open Application_types

let ( % ) f g x = f (g x)

module Markup = Page_input.Make(Xml)(Svg)(Html)

module Api_template = Api_cohttp_template.Make(User)

let common_tabs () = []

let tabs_of_cpu input = function
  | None -> []
  | Some process ->
    List.flatten
    @@ List.filter_map (function
        | `Input i, tabs ->
          if Topology.equal_topo_input i input
          then Some tabs else None
        | _ -> None) process#tabs

let tabs_of_board input (_control, (board : Boards.Board.t)) =
  List.flatten
  @@ List.filter_map (function
      | `Input i, tabs ->
        if Topology.equal_topo_input i input
        then Some tabs else None
      | _ -> None) board.gui_tabs

let cons_uniq ~eq l x = if List.exists (eq x) l then l else x :: l

let dedup ~eq l = List.rev (List.fold_left (cons_uniq ~eq) [] l)

let make_template
    (input : Topology.topo_input)
    (cpu : Data_processor.t option)
    (boards : Boards.Board.t Boards.Board.Map.t) =
  let tabs =
    common_tabs ()
    @ (List.flatten
       @@ List.map (tabs_of_board input)
       @@ Boards.Board.Map.bindings boards)
    @ tabs_of_cpu input cpu in
  let title = Topology.get_input_name input in
  let tabs, slides, stylesheets, pre_scripts, post_scripts =
    List.fold_left (fun (tabs, slides, css, pre_js, post_js) template ->
        let id = Netlib.Uri.Path.to_string template#path in
        let tab = Markup.make_tab ~id template#title in
        let slide = Markup.make_tabpanel ~id @@ Html.totl template#content in
        ( tab :: tabs
        , slide :: slides
        , template#stylesheets @ css
        , template#pre_scripts @ pre_js
        , template#post_scripts @ post_js))
      ( []
      , []
      , ["/css/page-input.min.css"]
      , []
      , [`Src "/js/page-input.js"])
    @@ List.rev tabs in
  let tab_bar = Markup.make_tab_bar tabs in
  let slides = Markup.make_content slides in
  let eq a b =
    match a, b with
    | `Src a, `Src b | `Raw a, `Raw b -> String.equal a b
    | _ -> false in
  Api_template.make_template_props
    ~title
    ~stylesheets:(dedup ~eq:String.equal stylesheets)
    ~pre_scripts:(dedup ~eq pre_scripts)
    ~post_scripts:(dedup ~eq post_scripts)
    ~top_app_bar_bottom:(Tyxml.Html.toelt tab_bar)
    ~content:[Tyxml.Html.toelt slides]
    ()
