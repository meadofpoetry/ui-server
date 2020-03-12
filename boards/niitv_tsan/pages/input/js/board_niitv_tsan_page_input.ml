open Js_of_ocaml
open Application_types
open Board_niitv_tsan_types
open Components

let () =
  let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let thread =
    try
      let input =
        Topology.topo_input_of_yojson
        @@ Yojson.Safe.from_string
        @@ Js.to_string Js.Unsafe.global##.input
      in
      let boards =
        Topology.boards_of_yojson
        @@ Yojson.Safe.from_string
        @@ Js.to_string Js.Unsafe.global##.boards
      in
      match (input, boards) with
      | Ok input, Ok boards ->
          let tsan_boards = List.assoc_opt board_id boards in
          Option.iter
            (fun controls ->
              let _pages =
                List.iter
                  (fun control ->
                    Board_niitv_tsan_page_pids.init ~input control;
                    Board_niitv_tsan_page_services.init ~input control;
                    Board_niitv_tsan_page_si_psi.init ~input control)
                  controls
              in
              ()
              (* scaffold#set_on_destroy (fun () -> List.iter Widget.destroy pages) *))
            tsan_boards;
          Lwt.return_ok ()
      | Error e, _ | _, Error e -> Lwt.return_error (`Msg e)
    with e -> Lwt.return_error (`Msg (Printexc.to_string e))
  in
  let _loader = Components_lab.Loader.make_loader ?elt:scaffold#body thread in
  ()
