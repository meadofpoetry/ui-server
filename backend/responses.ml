open Api.Interaction
module Widgets = Common.Components.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)
open Widgets

let fill_json ?(title="АТС-3") ?(scripts=[]) ?(stylesheets=[]) ?(content=[]) () =
  `O [ "title", `String title
     ; "scripts", `A (List.map (fun x -> `O [ "script", `String x ]) scripts)
     ; "stylesheets", `A (List.map (fun x -> `O [ "stylesheet", `String x ]) stylesheets)
     ; "content", `A (List.map (fun x -> `O [ "element", `String x]) content) ]

let render_with_template base template_path json =
  let tmpl = Filename.concat base template_path
             |> CCIO.File.read_exn (* FIXME *)
             |> Mustache.of_string in
  Mustache.render tmpl json

let render_with_base_template base json =
  render_with_template base "html/templates/base.html" json

module Settings = struct

  let title s = "Настройки - " ^ s

  let users base =
    let content = [ ] in
    (* List.map Common.Components.to_string (Page_users.users ()) *)
    let json    = fill_json ~title:(title "Пользователи")
                            ~content:content
                            () in
    let html    = render_with_base_template base json in
    respond_string html ()

end

let home base =
  let content = Tyxml.Html.(div [ p [pcdata "This is the main ATS-3 page"] ]
                            |> (Format.asprintf "%a" (pp_elt ()))) in
  let json = fill_json ~scripts:[ "/js/home.js"; "/js/janus.nojquery.js"; "/js/adapter.min.js" ]
                       ~content:[content]
                       () in
  let html = render_with_base_template base json in
  respond_string html ()

let configuration base =
  let json = fill_json ~scripts:[ "js/configuration.js" ]
                       ~content:[""]
                       () in
  let html = render_with_template base "html/templates/empty.html" json in
  respond_string html ()

let mdc_demo base =
  let content = "" in
  let json    = fill_json ~scripts:[ "/js/Chart.min.js"
                                   ; "/js/demo.js"
                                   ]
                          ~content:[content]
                          () in
  let html = render_with_template base "html/templates/empty.html" json in
  respond_string html ()
