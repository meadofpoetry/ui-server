open Containers
open Api.Template
open Common

module Icon = Components_markup.Icon.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)

let make_icon path =
  let open Icon.SVG in
  let path = create_path path () in
  let icon = create [path] () in
  Tyxml.Html.toelt icon

let get_input_href (x : Topology.topo_input) =
  let name = Topology.input_to_string x.input in
  let id = string_of_int x.id in
  Filename.concat name id

let input topo (input : Topology.topo_input) =
  let path =
    List.find_map (fun (i, p, c) ->
        if Topology.equal_topo_input i input
        then Some (p, c) else None)
      (Topology.get_paths topo) in
  match path with
  | None              -> failwith "input not found"
  | Some (boards, cpu) ->
     let title = Topology.get_input_name input in
     let boards =
       List.map (fun (x : Topology.topo_board) -> x.control, x.typ) boards
       |> Topology.boards_to_yojson
       |> Yojson.Safe.to_string in
     let cpu =
       Option.map (fun (x : Topology.topo_cpu) -> x.process) cpu
       |> Json.Option.to_yojson Topology.process_type_to_yojson
       |> Yojson.Safe.to_string in
     let input_string = Topology.Show_topo_input.to_string input in
     let input_template =
       { title = Some title
       ; pre_scripts  =
           [ Raw (Printf.sprintf "var input = \"%s\";\
                                  var boards = %s;\
                                  var cpu = %s;"
                    input_string boards cpu)]
       ; post_scripts = [Src "/js/input.js"]
       ; stylesheets = []
       ; content = []
       } in
     let input_page =
       `Index input.id,
       Simple { title
              ; icon = None
              ; href = Uri.Path.of_string @@ get_input_href input
              ; template = input_template } in
     let pre = "input/" ^ get_input_href input in
     let stream_template =
       { title = Some ("Входы / " ^ title)
       ; pre_scripts =
           [ Raw (Printf.sprintf "var input = \"%s\";\
                                  var boards = %s;\
                                  var cpu = %s;"
                    input_string boards cpu)
           ; Src "/js/moment.min.js"
           ; Src "/js/Chart.min.js"
           ; Src "/js/chartjs-plugin-deferred.min.js"
           ; Src "/js/Chart.PieceLabel.min.js"
           ]
       ; post_scripts = [Src "/js/stream.js"]
       ; stylesheets = []
       ; content = []
       } in
     let stream_page =
       `Index input.id,
       Pure { path = Uri.Path.Format.(pre @/ Stream.ID.fmt ^/ empty)
            ; template = stream_template } in
     input_page, stream_page

let create (app : Application.t) : upper ordered_item list User.user_table =
  let topo  = React.S.value app.topo in
  let hw_templates =
    Hardware.Map.fold (fun _ (x : Boards.Board.t) acc ->
        List.cons_maybe x.templates acc) app.hw.boards [] in
  let props =
    { title = Some "Конфигурация"
    ; pre_scripts = []
    ; post_scripts = [Src "js/topology.js"]
    ; stylesheets = ["/css/topology.min.css"]
    ; content = []
    } in
  let demo_props =
    { title = Some "UI Демо"
    ; pre_scripts = [ Src "/js/moment.min.js"
                    ; Src "/js/Chart.min.js"
                    ; Src "/js/chartjs-plugin-deferred.min.js"]
    ; post_scripts = [ Src "/js/demo.js" ]
    ; stylesheets = ["/css/demo.min.css"]
    ; content = []
    } in
  let inputs = Topology.get_inputs topo in
  let input_templates, stream_templates =
    List.map (input topo) inputs
    |> List.split in
  let app_template =
    [ `Index 2,
      Subtree { title = "Входы"
              ; icon = Some (make_icon Icon.SVG.Path.arrow_right_box)
              ; href = Uri.Path.of_string "input"
              ; templates = input_templates }
    ; `Index 3,
      Simple  { title = "Конфигурация"
              ; icon = Some (make_icon Icon.SVG.Path.tournament)
              ; href = Uri.Path.of_string "application"
              ; template = props }
    (* ; `Index 4,
     *   Simple  { title = "UI Демо"
     *           ; icon = Some (make_icon Icon.SVG.Path.material_design)
     *           ; href = Uri.Path.of_string "demo"
     *           ; template = demo_props } *)
    ]
  in
  let proc = match app.proc with
    | None -> Common.User.empty_table
    | Some p -> p#template ()
  in
  Common.User.concat_table
    ([ Responses.home_template ()
     ; User_template.create ()
     ; Pc_control.Network_template.create ()
     ; proc
     ; { root = app_template
       ; operator = app_template
       ; guest = app_template }
     ; { root = stream_templates
       ; operator = stream_templates
       ; guest = stream_templates }
     ]
     @ hw_templates)
