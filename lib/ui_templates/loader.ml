open Js_of_ocaml
open Js_of_ocaml_lwt
open Components

let ( >>= ) = Lwt.bind

let ( >>=? ) = Lwt_result.bind

module CSS = struct
  let root = "mdc-loader"
end

let timeout = 0.4

let exn_to_string : exn -> string = function
  | Failure s -> s
  | Invalid_argument s -> Printf.sprintf "Invalid argument: %s" s
  | Not_found -> "Not found"
  | Out_of_memory -> "Out of memory"
  | Stack_overflow -> "Stack overflow"
  | Sys_error s -> Printf.sprintf "Sys error: %s" s
  | Division_by_zero -> "Division by zero"
  | e -> Printexc.to_string e

let make_loader ?(text : string option)
    ?(error_icon : #Dom_html.element Js.t option)
    ?(on_error : (Dom_html.element Js.t -> string -> unit) option)
    ?(on_success : (Dom_html.element Js.t -> 'a -> unit) option)
    ?(elt = Dom_html.(createDiv document))
    (t : ('a, string) Lwt_result.t) =
  let progress = Placeholder.Progress.make ?text () in
  let on_success (v : 'a) : unit Lwt.t =
    Element.remove_class elt CSS.root;
    (match on_success with
     | None -> ()
     | Some f -> f elt v);
    Lwt.return_unit in
  let on_error text =
    let error = Placeholder.Err.make ?icon:error_icon ~text () in
    Element.append_child elt error#root;
    (match on_error with
     | None -> ()
     | Some f -> f elt text);
    Lwt.return_unit in
  Element.add_class elt CSS.root;
  List.iter (Element.remove_child_safe elt)
  @@ Element.query_selector_all elt ("." ^ Placeholder.CSS.root);
  let sleep =
    Lwt_js.sleep timeout
    >>= fun () ->
    Element.append_child elt progress#root;
    Lwt.return_unit in
  let thread =
    Lwt.try_bind
      (fun () -> t)
      (fun r ->
         Lwt.cancel sleep;
         Element.remove_child_safe elt progress#root;
         match r with
         | Error e -> on_error e
         | Ok x -> on_success x)
      (fun e ->
         Lwt.cancel sleep;
         Element.remove_child_safe elt progress#root;
         on_error @@ exn_to_string e) in
  Lwt.on_termination thread (fun () -> progress#destroy ());
  elt

let make_widget_loader ?text ?error_icon ?elt
    (t : (#Widget.t, string) Lwt_result.t) =
  make_loader ?text ?error_icon ?elt
    ~on_success:(fun elt widget ->
        Dom.appendChild elt widget#root;
        widget#layout ()) t
