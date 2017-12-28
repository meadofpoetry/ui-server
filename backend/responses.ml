open Api.Interaction
module Widgets = Common.Components.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)
open Widgets

type tmpl_props =
  { title        : string option
  ; pre_scripts  : string list
  ; post_scripts : string list
  ; stylesheets  : string list
  ; content      : string list
  }

module type Template = sig
  val topo : Common.Topology.topology
  val path : string
end

module Make (M : Template) = struct

  let elt_to_string x = Tyxml.Html.(Format.asprintf "%a" (pp_elt ()) x)

  let get_input_name x inputs =
    let open Common.Topology in
    let single = CCList.find_pred (fun input -> input.input = x.input && input.id <> x.id)
                                  inputs
                 |> CCOpt.is_none in
    let to_string s = if single then Printf.sprintf "%s" s else Printf.sprintf "%s %d" s x.id in
    match x.input with
    | RF    -> to_string "RF"
    | TSOIP -> to_string "TSoIP"
    | ASI   -> to_string "ASI"

  let get_input_href x =
    let open Common.Topology in
    let name = input_to_string x.input in
    let id   = string_of_int x.id in
    Filename.concat name id

  let input_props =
    let inputs = Hardware.topo_inputs M.topo in
    CCList.map (fun x -> get_input_href x,get_input_name x inputs) inputs

  let fill_json props =
    `O [ "title", `String (CCOpt.get_or ~default:"АТС-3" props.title)
       ; "pre_scripts", `A (List.map (fun x -> `O [ "script", `String x ]) props.pre_scripts)
       ; "post_scripts", `A (List.map (fun x -> `O [ "script", `String x ]) props.post_scripts)
       ; "stylesheets", `A (List.map (fun x -> `O [ "stylesheet", `String x ]) props.stylesheets)
       ; "inputs", `A (List.map (fun (h,n) -> `O [ "input", `String n
                                                 ; "href",  `String ("/input/" ^ h)]) input_props)
       ; "content", `A (List.map (fun x -> `O [ "element", `String x]) props.content) ]

  let template ?(tmpl="base.html") path props =
    let tmpl = Filename.concat path ("html/templates/" ^ tmpl)
               |> CCIO.File.read_exn (* FIXME *)
               |> Mustache.of_string in
    Mustache.render tmpl (fill_json props)

  let home () =
    let content = Tyxml.Html.(div [ p [pcdata "This is the main ATS-3 page"] ]
                              |> (Format.asprintf "%a" (pp_elt ())))
    in
    let props = { title        = None
                ; pre_scripts  = [ "/js/janus.nojquery.js"; "/js/adapter.min.js" ]
                ; post_scripts = [ "/js/home.js" ]
                ; stylesheets  = []
                ; content      = [content]
                } in
    respond_string (template M.path props) ()

  let pipeline () =
    let props = { title        = None
                ; pre_scripts  = [ "/js/janus.nojquery.js"; "/js/adapter.min.js" ]
                ; post_scripts = [ "/js/pipeline.js" ]
                ; stylesheets  = []
                ; content      = []
                } in
    respond_string (template M.path props) ()

  let hardware () =
    let props = { title        = None
                ; pre_scripts  = []
                ; post_scripts = [ "js/hardware.js" ]
                ; stylesheets  = []
                ; content      = []
                } in
    respond_string (template M.path props) ()

  let demo () =
    let props = { title        = None
                ; pre_scripts  = [ "/js/moment.min.js"; "/js/Chart.min.js" ]
                ; post_scripts = [ "/js/demo.js" ]
                ; stylesheets  = []
                ; content      = []
                } in
    respond_string (template ~tmpl:"empty.html" M.path props) ()

end
