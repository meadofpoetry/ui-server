open Lwt.Infix
open Hardware_js

open Common.Topology

let t =
  [Board
     {
       typ = TS2IP
     ; model = "CONV"
     ; manufacturer = "TRI (NIITV)"
     ; version = 3
     ; control = 1
     ; connection = `Fine
     ; ports = [
         {
           port = 1;
           listening = true;
           child = Board
                     {
                       typ = TS
                     ; model = "ANALYSIS"
                     ; manufacturer = "TRI (NIITV)"
                     ; version = 3
                     ; control = 1
                     ; connection = `Init
                     ; ports = [
                         {
                           port = 1;
                           listening = true;
                           child = Board
                                     {
                                       typ = DVB
                                     ; model = "DVB"
                                     ; manufacturer = "TRI (NIITV)"
                                     ; version = 3
                                     ; control = 1
                                     ; connection = `Fine
                                     ; ports =  [
                                         {
                                           port = 1
                                         ; listening = false
                                         ; child = Input { input = RF; id = 1 }
                                         }
                                       ]
                                     }
                         };
                         {
                           port = 2
                         ; listening = false
                         ; child = Input { input = ASI; id = 1}
                       }]
                     }
         };
         {
           port = 2
         ; listening = false
         ; child =
             Board
               {
                 typ = TS
               ; model = "ANALYSIS"
               ; manufacturer = "TRI (NIITV)"
               ; version = 3
               ; control = 1
               ; connection = `No_response
               ; ports = [
                   {
                     port = 1;
                     listening = false;
                     child = Board
                               {
                                 typ = DVB
                               ; model = "DVB"
                               ; manufacturer = "TRI (NIITV)"
                               ; version = 3
                               ; control = 1
                               ; connection = `Init
                               ; ports =  [
                                   {
                                     port = 1
                                   ; listening = false
                                   ; child = Input { input = RF; id = 2}
                                   }
                                 ]
                               }
                   };
                   {
                     port = 2
                   ; listening = true
                   ; child = Input { input = ASI; id = 2}
                 }]
               }
         };
         {
           port = 3
         ; listening = false
         ; child =
             Board
               {
                 typ = TS
               ; model = "ANALYSIS"
               ; manufacturer = "TRI (NIITV)"
               ; version = 3
               ; control = 1
               ; connection = `Fine
               ; ports = [
                   {
                     port = 1;
                     listening = false;
                     child = Board
                               {
                                 typ = IP2TS
                               ; model = "DVB"
                               ; manufacturer = "TRI (NIITV)"
                               ; version = 3
                               ; control = 1
                               ; connection = `Fine
                               ; ports =  [
                                   {
                                     port = 1
                                   ; listening = false
                                   ; child = Input { input = TSOIP; id = 1}
                                   }
                                 ]
                               }
                   };
                   {
                     port = 2
                   ; listening = true
                   ; child = Input { input = ASI; id = 3 }
                 }]
               }
         };
         {
           port = 4
         ; listening = false
         ; child =
             Board
               {
                 typ = TS
               ; model = "ANALYSIS"
               ; manufacturer = "TRI (NIITV)"
               ; version = 3
               ; control = 1
               ; connection = `Fine
               ; ports = [
                   {
                     port = 1;
                     listening = true;
                     child = Board
                               {
                                 typ = IP2TS
                               ; model = "DVB"
                               ; manufacturer = "TRI (NIITV)"
                               ; version = 3
                               ; control = 1
                               ; connection = `Fine
                               ; ports =  [
                                   {
                                     port = 1
                                   ; listening = false
                                   ; child = Input { input = TSOIP; id = 2}
                                   }
                                 ]
                               }
                   };
                   {
                     port = 2
                   ; listening = true
                   ; child = Input { input = ASI; id = 4}
                 }]
               }
         }
       ]
  }]

let onload _ =
  let doc = Dom_html.document in

  let canvas = Dom_html.createCanvas doc in
  (* Topology.render ~topology:t ~width:1500 ~height:1000 ~canvas ~on_click:None; *)
  Dom.appendChild doc##.body canvas;
  Requests.get_topology ()
  >>= (fun resp -> match resp with
                   | Ok t ->
                      topology_to_yojson t
                      |> Yojson.Safe.pretty_to_string
                      |> print_endline;
                      Lwt.return @@ Topology.render ~topology:t ~canvas ~width:900 ~height:900 ()
                   | Error e -> Lwt.return @@ print_endline e)
  |> ignore;
  let _ = React.E.map (fun x -> Topology.render ~topology:x ~canvas ~width:900 ~height:900 ())
                      (Requests.get_topology_socket ()) in
  Js._false

let () = Dom_html.addEventListener Dom_html.document
                                   Dom_events.Typ.domContentLoaded
                                   (Dom_html.handler onload)
                                   Js._false
         |> ignore
