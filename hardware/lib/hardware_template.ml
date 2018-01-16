open Common.Topology
open Api.Template

let get_input_name x topo =
  let single = CCList.find_pred (fun input -> input.input = x.input && input.id <> x.id)
                                (Hardware.topo_inputs topo)
               |> CCOpt.is_none in
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
  let path = CCList.find_map (fun (i,p) -> if i = topo_input then Some p else None)
                              (Hardware.topo_paths topo) in
  match path with
  | None      -> failwith "input not found"
  | Some path -> let title  = get_input_name topo_input topo in
                 let boards = CCList.map (fun x -> x.control,x.typ ) path
                              |> boards_to_yojson
                              |> Yojson.Safe.to_string
                 in
                 let template = { title        = Some ("Вход " ^ title)
                                ; pre_scripts  = [ Raw (Printf.sprintf "var boards = %s" boards) ]
                                ; post_scripts = [ Src "/js/input.js" ]
                                ; stylesheets  = []
                                ; content      = []
                                } in
                 { title; href = get_input_href topo_input; template }

let create (hw : Hardware.t) =
  let topo  = React.S.value hw.topo in
  let props = { title        = None
              ; pre_scripts  = []
              ; post_scripts = [ Src "js/hardware.js" ]
              ; stylesheets  = []
              ; content      = []
              } in
  let templates = CCList.map (input topo) (Hardware.topo_inputs topo) |> CCList.rev in
  [ Subtree { title = "Входы"; href = "input"; templates }
  ; Simple  { title = "Конфигурация"; href = "hardware"; template = props }
  ; Ref     ("Wiki","//wikipedia.org")
  ]
