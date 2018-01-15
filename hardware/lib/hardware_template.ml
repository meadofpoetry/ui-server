open Common.Topology
open Api.Template

(*let inputs = Hardware.topo_inputs M.topo

let get_input_name x =
  let single = CCList.find_pred (fun input -> input.input = x.input && input.id <> x.id)
                 inputs
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

let input_props = CCList.map (fun x -> get_input_href x,get_input_name x) inputs

let topo_paths = Hardware.topo_paths M.topo


let create () =
  let props = { title        = None
              ; pre_scripts  = []
              ; post_scripts = [ Src "js/hardware.js" ]
              ; stylesheets  = []
              ; content      = []
              } in
  respond_string (template M.path props) ()

let input (topo_input:topo_input) () =
  let path  = CCList.find_map (fun (i,p) -> if i = topo_input then Some p else None) topo_paths in
  match path with
  | Some path -> let boards = CCList.map (fun x -> x.control,x.typ ) path
                              |> boards_to_yojson
                              |> Yojson.Safe.to_string
                 in
                 let props  = { title        = Some ("Вход " ^ (get_input_name topo_input))
                              ; pre_scripts  = [ Raw (Printf.sprintf "var boards = %s" boards) ]
                              ; post_scripts = [ Src "/js/input.js" ]
                              ; stylesheets  = []
                              ; content      = []
                              } in
                 respond_string (template M.path props) ()
  | None      -> respond_error "This input does not exist" ()
 *)

let create topo : Api.Template.item list = [Ref ("topo", "topo")]
