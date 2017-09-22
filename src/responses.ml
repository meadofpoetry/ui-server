open Interaction
module Widgets = Common.Components.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)
open Widgets

let fill_json ?(title="АТС-3") ?(scripts=[]) ?(stylesheets=[]) ?(content=[]) () =
  `O [ "title", `String title
     ; "scripts", `A (List.map (fun x -> `O [ "script", `String x ]) scripts)
     ; "stylesheets", `A (List.map (fun x -> `O [ "stylesheet", `String x ]) stylesheets)
     ; "content", `A (List.map (fun x -> `O [ "element", `String x]) content) ]

let render_with_base_template base json =
  let tmpl = Filename.concat base "html/templates/base.html"
             |> CCIO.File.read_exn (* FIXME *)
             |> Mustache.of_string in
  Mustache.render tmpl json

module Settings = struct

  let title s = "Настройки - " ^ s

  let users base =
    let content = List.map Common.Components.to_string (Page_users.users ()) in
    let json    = fill_json ~title:(title "Пользователи")
                            ~content:content
                            () in
    let html    = render_with_base_template base json in
    respond_string html ()

end

let home base =
  let content = Tyxml.Html.(div [ p [pcdata "This is the main ATS-3 page"]
                                ; video ~a:[ a_id "remotevideo"
                                           ; a_width 640
                                           ; a_autoplay () ] []
                                ]
                            |> (Format.asprintf "%a" (pp_elt ()))) in
  let json = fill_json ~scripts:[ "/js/home.js"; "/js/janus.nojquery.js"; "/js/adapter.min.js" ]
                       ~content:[content]
                       () in
  let html = render_with_base_template base json in
  respond_string html ()
