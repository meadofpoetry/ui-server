open Tyxml
open Boards
open Application_types
module Api_template = Api_cohttp_template.Make (User)

let ( % ) f g x = f (g x)

let common_tabs () = []

let tabs_of_cpu input = function
  | None -> []
  | Some process ->
      List.flatten
      @@ List.filter_map
           (function
             | `Input i, tabs ->
                 if Topology.equal_topo_input i input then Some tabs else None
             | _ -> None)
           process#tabs

let tabs_of_board input (board : Boards.Board.t) =
  List.flatten
  @@ List.filter_map
       (function
         | `Input i, tabs ->
             if Topology.equal_topo_input i input then Some tabs else None
         | _ -> None)
       board.gui_tabs

let cons_uniq ~eq l x = if List.exists (eq x) l then l else x :: l

let dedup ~eq l = List.rev (List.fold_left (cons_uniq ~eq) [] l)

module Boards = Map.Make (struct
  type t = Topology.board_id

  let compare = Topology.compare_board_id
end)

let make_template
    (input : Topology.topo_input)
    (cpu : Data_processor.t option)
    (boards : Board.t Board.Map.t) =
  let board_tabs =
    List.flatten
    @@ List.map (fun (_, b) -> tabs_of_board input b)
    @@ Board.Map.bindings boards
  in
  let boards_var =
    Board.Map.fold
      (fun control (board : Board.t) acc ->
        let has_input =
          List.exists
            (function
              | `Input i, _ -> Topology.equal_topo_input input i
              | _ -> false)
            board.gui_tabs
        in
        if has_input
        then
          Boards.update
            board.id
            (function
              | None -> Some [control]
              | Some acc -> Some (control :: acc))
            acc
        else acc)
      boards
      Boards.empty
    |> Boards.bindings
    |> Topology.boards_to_yojson
    |> Yojson.Safe.pretty_to_string
  in
  let tabs = common_tabs () @ board_tabs @ tabs_of_cpu input cpu in
  let title = Topology.get_input_name input in
  let tabs, slides, stylesheets, pre_scripts, post_scripts =
    List.fold_left
      (fun (tabs, slides, css, pre_js, post_js) template ->
        let id =
          String.map (function
              | '/' -> '-'
              | c -> c)
          @@ Netlib.Uri.Path.to_string template#path
        in
        let tab_id = id ^ "-tab" in
        let tab =
          Page_input_tyxml.Markup.make_tab ~id:tab_id ~controls:id template#title
        in
        let slide =
          Page_input_tyxml.Markup.make_tabpanel ~id ~labelledby:tab_id
          @@ Html.totl template#content
        in
        (* FIXME scripts order *)
        ( tabs @ [tab]
        , slides @ [slide]
        , css @ template#stylesheets
        , pre_js @ template#pre_scripts
        , post_js @ template#post_scripts ))
      ( []
      , []
      , ["/css/page-input.min.css"]
      , [`Raw (Printf.sprintf "var boards = `%s`;" boards_var)]
      , [`Src "/js/page-input.js"] )
      tabs
  in
  let tab_bar = Page_input_tyxml.Markup.make_tab_bar tabs in
  let slides = Page_input_tyxml.Markup.make_content slides in
  let eq a b =
    match a, b with
    | `Src a, `Src b | `Raw a, `Raw b -> String.equal a b
    | _ -> false
  in
  Api_template.make_template_props
    ~title
    ~stylesheets:(dedup ~eq:String.equal stylesheets)
    ~pre_scripts:(dedup ~eq pre_scripts)
    ~post_scripts:(dedup ~eq post_scripts)
    ~top_app_bar_bottom:(Tyxml.Html.toelt tab_bar)
    ~content:[Tyxml.Html.toelt slides]
    ()
