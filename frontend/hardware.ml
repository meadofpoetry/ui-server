open Lwt.Infix
open Hardware_js
open Common.Topology

let () =
  let doc = Dom_html.document in
  let ac  = Dom_html.getElementById "arbitrary-content" in

  let canvas = Dom_html.createCanvas doc in
  let width () = canvas##.parentNode
                 |> Js.Opt.to_option |> CCOpt.get_exn
                 |> Js.Unsafe.coerce
                 |> (fun x -> x##.offsetWidth) in
  Dom.appendChild ac canvas;
  Requests.get_topology ()
  >>= (fun resp -> match resp with
                   | Ok t    -> Lwt.return @@ Topology.render ~topology:t
                                                              ~canvas
                                                              ~width:(width ())
                                                              ~on_click:(fun x -> (match x with
                                                                                   | Input i -> (match i.input with
                                                                                                 | ASI -> "asi "
                                                                                                 | RF  -> "rf "
                                                                                                 | TSOIP -> "tsoip ")
                                                                                                ^ (string_of_int i.id)
                                                                                   | Board b -> b.model)
                                                                                  |> print_endline)
                                                              ()
                   | Error e -> Lwt.return @@ print_endline e)
  |> ignore;
  React.E.map (fun x -> Topology.render ~topology:x ~canvas ~width:(width ()) ())
              (Requests.get_topology_socket ())
  |> ignore
