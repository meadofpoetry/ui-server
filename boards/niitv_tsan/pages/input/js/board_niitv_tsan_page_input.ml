open Js_of_ocaml
open Application_types
open Board_niitv_tsan_types
open Components

let () =
  let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let boards =
    Topology.boards_of_yojson
    @@ Yojson.Safe.from_string
    @@ Js.to_string Js.Unsafe.global##.boards
  in
  match boards with
  | Error _ -> ()
  | Ok boards ->
      let tsan_boards = List.assoc_opt board_id boards in
      Option.iter
        (fun controls ->
          let pages =
            List.filter_map
              (fun control -> Board_niitv_tsan_page_pids.init control)
              controls
          in
          scaffold#set_on_destroy (fun () -> List.iter Widget.destroy pages))
        tsan_boards
