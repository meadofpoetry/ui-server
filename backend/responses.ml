open Api.Interaction
module Widgets = Common.Components.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)
open Widgets

type script = Src of string
            | Raw of string

type tmpl_props =
  { title        : string option
  ; pre_scripts  : script list
  ; post_scripts : script list
  ; stylesheets  : string list
  ; content      : string list
  }

module type Template = sig
  val topo : Common.Topology.topology
  val path : string
end

module Make (M : Template) = struct

  open Common.Topology

  let elt_to_string x = Tyxml.Html.(Format.asprintf "%a" (pp_elt ()) x)

  let inputs = Hardware.topo_inputs M.topo

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

  let fill_json props =
    let script_to_object = function
      | Raw s -> `O [ "script", `String s; "src", `Bool false ]
      | Src s -> `O [ "script", `String s; "src", `Bool true  ] in
    `O [ "title",        `String (CCOpt.get_or ~default:"АТС-3" props.title)
       ; "pre_scripts",  `A (List.map script_to_object props.pre_scripts)
       ; "post_scripts", `A (List.map script_to_object props.post_scripts)
       ; "stylesheets",  `A (List.map (fun x -> `O [ "stylesheet", `String x ]) props.stylesheets)
       ; "inputs",       `A (List.map (fun (h,n) -> `O [ "input", `String n
                                                       ; "href",  `String ("/input/" ^ h)]) input_props)
       ; "content",      `A (List.map (fun x -> `O [ "element", `String x]) props.content) ]

  let template ?(tmpl="base.html") path props =
    let tmpl = Filename.concat path ("html/templates/" ^ tmpl)
               |> CCIO.File.read_exn (* FIXME *)
               |> Mustache.of_string in
    try
      Mustache.render tmpl (fill_json props)
    with
    | e -> Printexc.to_string e

  let home () =
    let content = Tyxml.Html.(div [ p [pcdata "This is the main ATS-3 page"] ]
                              |> (Format.asprintf "%a" (pp_elt ())))
    in
    let props = { title        = None
                ; pre_scripts  = [ Src "/js/janus.nojquery.js"; Src "/js/adapter.min.js" ]
                ; post_scripts = [ Src "/js/home.js" ]
                ; stylesheets  = []
                ; content      = [content]
                } in
    respond_string (template M.path props) ()

  let pipeline () =
    let props = { title        = None
                ; pre_scripts  = [ Src "/js/janus.nojquery.js"; Src "/js/adapter.min.js" ]
                ; post_scripts = [ Src "/js/pipeline.js" ]
                ; stylesheets  = []
                ; content      = []
                } in
    respond_string (template M.path props) ()

  let hardware () =
    let props = { title        = None
                ; pre_scripts  = []
                ; post_scripts = [ Src "js/hardware.js" ]
                ; stylesheets  = []
                ; content      = [ ]
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

  let demo () =
    let props = { title        = None
                ; pre_scripts  = [ Src "/js/moment.min.js"; Src "/js/Chart.min.js" ]
                ; post_scripts = [ Src "/js/demo.js" ]
                ; stylesheets  = []
                ; content      = []
                } in
    respond_string (template ~tmpl:"empty.html" M.path props) ()

end
