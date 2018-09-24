open Board_qos_niit_js
open Common.Uri
open Common.Stream
open Common.Topology
open Containers
open Lwt_result.Infix
open Api_js.Api_types
open Components

let get_stream id control =
  Requests.History.HTTP.Streams.get
    ~compress:true
    ~ids:[id] control

let () =
  let arbitrary = Dom_html.getElementById "arbitrary-content"
                  |> Widget.create in
  let uri = Dom_html.window##.location##.pathname |> Js.to_string in
  (* TODO this format can be defined somewhere in common code *)
  let fmt = Path.Format.("board" @/ Int ^/ "stream" @/ ID.fmt ^/ empty) in
  let control, id =
    Path.Format.scan_unsafe (Path.of_string uri) fmt Pair.make in
  get_stream id control
  >>= (function
       | Compressed x ->
          begin match x.data with
          | [x] -> Lwt_result.return (fst x)
          | _ -> Lwt.fail_with "no such stream or more than one stream"
          end
       | Raw _ -> assert false)
  >|= (fun stream ->
    let input = match get_input stream with
      | Some x -> "Входы / " ^ (get_input_name x) ^ " / "
      | None -> "" in
    let page = Stream_page.make stream control in
    let info = Source.to_string stream.source.info in
    page#set_title @@ Printf.sprintf "%s%s" input info)
  |> Lwt_result.map_err Api_js.Requests.err_to_string
  |> Ui_templates.Loader.create_loader
       ~on_success:(fun w _ -> arbitrary#remove_child w)
  |> fun w -> arbitrary#append_child w
