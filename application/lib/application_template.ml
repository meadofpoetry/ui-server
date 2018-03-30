open Containers
open Common.Topology
open Common.User
open Api.Template
open Api

let get_input_name x topo =
  let single = List.find_pred (fun input -> input.input = x.input && input.id <> x.id)
                                (Common.Topology.inputs topo)
               |> Option.is_none in
  let to_string s = if single then Printf.sprintf "%s" s else Printf.sprintf "%s %d" s x.id in
  match x.input with
  | RF    -> to_string "RF"
  | TSOIP -> to_string "TSoIP"
  | ASI   -> to_string "ASI"

let get_input_href x =
  let name = input_to_string x.input in
  let id   = string_of_int x.id in
  Filename.concat name id

let input topo (topo_input:topo_input) =
  let path = List.find_map (fun (i,p,_) -> if i = topo_input then Some p else None)
                             (Common.Topology.paths topo) in
  match path with
  | None      -> failwith "input not found"
  | Some path -> let title  = get_input_name topo_input topo in
                 let boards = List.map (fun x -> x.control,x.typ ) path
                              |> boards_to_yojson
                              |> Yojson.Safe.to_string
                 in
                 let template = { title        = Some ("Вход " ^ title)
                                ; pre_scripts  = [ Raw (Printf.sprintf "var boards = %s" boards)
                                                 ; Src "/js/moment.min.js"
                                                 ; Src "/js/Chart.min.js" ]
                                ; post_scripts = [ Src "/js/input.js" ]
                                ; stylesheets  = []
                                ; content      = []
                                } in
                 `Index topo_input.id, Simple { title; href = Path.of_string @@ get_input_href topo_input; template }

let create (app : Application.t) : upper ordered_item list user_table =
  let topo  = React.S.value app.topo in
  let props = { title        = Some "Конфигурация"
              ; pre_scripts  = []
              ; post_scripts = [ Src "js/topology.js" ]
              ; stylesheets  = [ "/css/topology.css" ]
              ; content      = []
              } in
  let demo_props = { title        = Some "Демо"
                   ; pre_scripts  = [ Src "/js/moment.min.js"
                                    ; Src "/js/Chart.min.js" ]
                   ; post_scripts = [ Src "/js/demo.js" ]
                   ; stylesheets  = [ ]
                   ; content      = [ ]
                   }
  in
  let templates = CCList.map (input topo) (Common.Topology.inputs topo) |> CCList.rev in
  let rval = [ `Index 2, Subtree { title = "Входы"; href = Path.of_string "input"; templates }
             ; `Index 3, Simple  { title = "Конфигурация"; href = Path.of_string "application"; template = props }
             ; `Index 4, Simple  { title = "Демо"; href = Path.of_string "demo"; template = demo_props }
             ]
  in
  let proc = match app.proc with
    | None -> Common.User.empty_table
    | Some p -> p#template ()
  in
  Common.User.concat_table [ Responses.home_template ()
                           ; User_template.create ()
                           ; proc
                           ; { root = rval; operator = rval; guest = rval }
    ]
