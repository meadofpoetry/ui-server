open Containers
open Components
open Common.Uri
open Common.Topology
open Common.Stream

let cpu_of_yojson =
  Common.Json.(
    Option.of_yojson String.of_yojson)

let boards_of_yojson =
  Common.Json.(
    List.of_yojson @@ Pair.of_yojson Int.of_yojson String.of_yojson)

let dummy_tab = fun () ->
  let div = Widget.create_div () in
  Ui_templates.Placeholder.under_development ()

let get_board_tabs (stream : Common.Stream.ID.t)
      (input : topo_input)
      (control : int)
      (name : string) =
  match name with
  | "TS" ->
     let open Board_qos_niit_js in
     let log =
       "QoS",
       "qos_log",
       (fun () -> Page_log.make stream control) in
     let services =
       "Сервисы",
       "qos_services",
       (fun () -> Page_services.make stream control) in
     let pids =
       "PIDs",
       "qos_pids",
       (fun () -> Page_pids.make stream control) in
     let tables =
       "SI/PSI",
       "qos_tables",
       (fun () -> Page_tables.make stream control) in
     [log; services; pids; tables]
  | "DVB" ->
     [ "RF", "rf", dummy_tab
     ]
  | _ -> []

let make_tabs stream input (boards : (int * string) list) =
  let tabs =
    List.flat_map (fun (control, name) ->
        get_board_tabs stream input control name) boards in
  List.map (fun (name, hash, f) ->
      new Tab.t ~value:(hash, f) ~content:(Text name) ()) tabs

let () =
  let open Common.Stream in
  let uri = Dom_html.window##.location##.pathname
            |> Js.to_string in
  let fmt = Path.Format.("input" @/ String ^/ Int ^/ ID.fmt ^/ empty) in
  let (input : topo_input) =
    Js.Unsafe.global##.input
    |> Js.to_string
    |> Show_topo_input.of_string in
  let (boards : (int * string) list) =
    Yojson.Safe.from_string @@ Js.to_string
    @@ Json.output @@ Js.Unsafe.global##.boards
    |> boards_of_yojson
    |> Result.get_exn in
  let (cpu : string option) =
    Yojson.Safe.from_string @@ Js.to_string
    @@ Json.output @@ Js.Unsafe.global##.cpu
    |> cpu_of_yojson
    |> Result.get_exn in
  let stream =
    Path.Format.scan_unsafe (Path.of_string uri) fmt (fun _ _ c -> c) in
  let w =
    Widget.create_div () in
  let _ = new Ui_templates.Page.t (`Dynamic (make_tabs stream input boards)) () in
  ()
