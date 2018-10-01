open Common.Uri
open Common.Stream

let () =
  let uri = Dom_html.window##.location##.pathname
            |> Js.to_string in
  let fmt = Path.Format.("input" @/ String ^/ Int ^/ ID.fmt ^/ empty) in
  let input_name, input_id, stream_id =
    Path.Format.scan_unsafe (Path.of_string uri) fmt (fun a b c -> a, b, c) in
  Printf.printf "name: %s, id: %d, stream: %s\n"
    input_name
    input_id
    (ID.to_string stream_id)
