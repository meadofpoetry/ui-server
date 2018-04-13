open Containers
open Components

let create_progress_block ?(text="Загрузка") () =
  let make_dot () = let dot = Dom_html.createSpan Dom_html.document |> Widget.create in
                    dot#set_text_content ".";
                    dot
  in
  let _class = "ats-progress-block" in
  let w  = new Circular_progress.t ~indeterminate:true () in
  let p  = Dom_html.createP Dom_html.document |> Widget.create in
  let () = p#set_text_content text in
  let () = List.iter (fun _ -> Dom.appendChild p#root (make_dot ())#root) @@ List.range' 0 3 in
  let b  = new Box.t ~vertical:true ~widgets:[w#widget;p#widget] () in
  let () = b#add_class _class in
  b#widget

let create_progress_block_lwt ?text ?error_icon ?error_prefix (t:('a,string) Lwt_result.t) =
  let open Lwt.Infix in
  let pgs = create_progress_block ?text () in
  let box = Dom_html.createDiv Dom_html.document |> Widget.create in
  Dom.appendChild box#root pgs#root;
  t >>= (fun r ->
    Dom.removeChild box#root pgs#root;
    match r with
    | Ok _ -> Lwt.return_unit
    | Error e -> let s = (match error_prefix with
                          | Some pfx -> Printf.sprintf "%s:\n %s" pfx e
                          | None     -> e)
                 in
                 let error = Error.create_error_block ?icon:error_icon s in
                 Dom.appendChild box#root error#root;
                 Lwt.return_unit)
  |> Lwt.ignore_result;
  box

