open Js_of_ocaml
open Application_types
open Board_niitv_tsan_types

let make_rate () =
  let br_4096 = Random.int 10000000 in
  let br_1000 = Random.int 5000000 in
  let br_2000 = Random.int 2000000 in
  let br_3000 = Random.int 1000000 in
  { Bitrate.
    timestamp = Ptime_clock.now ()
  ; pids = [ 4096, br_4096
           ; 1000, br_1000
           ; 2000, br_2000
           ; 3000, br_3000
           ]
  ; total = 20499000
  ; tables = []
  }

let initialize id control =
  let id =
    String.map (function '/' -> '-' | c -> c)
    @@ Topology.make_board_path id control in
  let elt = Dom_html.getElementById id in
  let pie = Board_niitv_tsan_widgets.Pid_bitrate_pie_chart.make
      ~rate:(make_rate ())
      () in
  let _ = Lwt_react.E.from (fun () ->
      Lwt.Infix.(
        Js_of_ocaml_lwt.Lwt_js.sleep 1.
        >>= fun () ->
        pie#set_rate (Some (make_rate ()));
        Lwt.return_unit)) in
  Dom.appendChild elt pie#root

let () =
  let boards =
    Topology.boards_of_yojson
    @@ Yojson.Safe.from_string
    @@ Js.to_string Js.Unsafe.global##.boards in
  match boards with
  | Error _ -> ()
  | Ok boards ->
    match List.find_opt (Topology.equal_board_id board_id % fst) boards with
    | None -> ()
    | Some (id, controls) -> List.iter (initialize id) controls
