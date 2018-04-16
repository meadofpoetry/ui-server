open Containers
open Components
open Lwt.Infix

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

class ['a] loader ?text ?error_icon ?error_prefix (t:('a,string) Lwt_result.t) () =
object(self)
  val pgs = create_progress_block ?text ()
  inherit Widget.widget (Dom_html.createDiv Dom_html.document) ()

  method progress = pgs
  method thread   = t

  initializer
    t >>= (fun r ->
    Dom.removeChild self#root pgs#root;
    match r with
    | Ok _    -> Lwt.return_unit
    | Error e -> let s = (match error_prefix with
                          | Some pfx -> Printf.sprintf "%s:\n %s" pfx e
                          | None     -> e)
                 in
                 let error = Error.create_error_block ?icon:error_icon s in
                 Dom.appendChild self#root error#root;
                 Lwt.return_unit)
    |> Lwt.ignore_result

end

class widget_loader ?text ?error_icon ?error_prefix (t:(Widget.widget,string) Lwt_result.t) () =
object(self)
  inherit [Widget.widget] loader ?text ?error_icon ?error_prefix t ()
  initializer
    Lwt_result.bind t (fun w -> Lwt_result.return @@ Dom.appendChild self#root w#root)
    |> Lwt.ignore_result
end

let create_progress_block_lwt ?(replace=true)
                              ?text
                              ?error_icon
                              ?error_prefix
                              (t:(#Widget.widget,string) Lwt_result.t) =
  let open Lwt.Infix in
  let pgs = create_progress_block ?text () in
  let box = Dom_html.createDiv Dom_html.document |> Widget.create in
  Dom.appendChild box#root pgs#root;
  t >>= (fun r ->
    Dom.removeChild box#root pgs#root;
    match r with
    | Ok x    -> if replace then Dom.appendChild box#root x#root; Lwt.return_unit
    | Error e -> let s = (match error_prefix with
                          | Some pfx -> Printf.sprintf "%s:\n %s" pfx e
                          | None     -> e)
                 in
                 let error = Error.create_error_block ?icon:error_icon s in
                 Dom.appendChild box#root error#root;
                 Lwt.return_unit)
  |> Lwt.ignore_result;
  box

