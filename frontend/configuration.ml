open Lwt.Infix
open Hardware_js

open Common.Topology

let onload _ =
  let doc = Dom_html.document in

  let canvas = Dom_html.createCanvas doc in
  let width () = canvas##.parentNode
                 |> Js.Opt.to_option |> CCOpt.get_exn
                 |> Js.Unsafe.coerce
                 |> (fun x -> x##.offsetWidth) in
  Dom.appendChild doc##.body canvas;
  Requests.get_topology ()
  >>= (fun resp -> match resp with
                   | Ok t    -> Lwt.return @@ Topology.render ~topology:t
                                                              ~canvas
                                                              ~width:(width ())
                                                              ~on_click:(fun _ -> print_endline "entry!")
                                                              ()
                   | Error e -> Lwt.return @@ print_endline e)
  |> ignore;
  let _ = React.E.map (fun x -> Topology.render ~topology:x ~canvas ~width:(width ()) ())
                      (Requests.get_topology_socket ()) in
  Js._false

let () = Dom_html.addEventListener Dom_html.document
                                   Dom_events.Typ.domContentLoaded
                                   (Dom_html.handler onload)
                                   Js._false
         |> ignore
