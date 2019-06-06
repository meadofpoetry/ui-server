open Js_of_ocaml
open Js_of_ocaml_lwt
open Components

let ( >>= ) = Lwt.( >>= )

let base_class = "mdc-loader"

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

class ['a] loader
    ?(text : string option)
    ?(error_icon : #Widget.t option)
    ?(error_prefix : string option)
    ?(on_error : ('a loader -> string -> unit) option)
    ?(on_success : ('a loader -> 'a -> unit) option)
    (t : ('a,string) Lwt_result.t)
    () =
  object(self)
    val progress = Placeholder.Progress.make ?text ()
    val mutable _on_success = on_success
    val mutable _on_error = on_error
    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      self#add_class base_class;
      let sleep =
        Lwt_js.sleep timeout
        >>= fun () ->
        self#append_child progress;
        Lwt.return_unit in
      Lwt.try_bind
        (fun () -> t)
        (fun r ->
           Lwt.cancel sleep;
           self#remove_child progress;
           match r with
           | Error e -> self#_on_error e
           | Ok x -> match _on_success with
             | None -> Lwt.return_unit
             | Some f -> Lwt.return @@ f (self :> 'a loader) x)
        (fun e ->
           Lwt.cancel sleep;
           self#remove_child progress;
           self#_on_error @@ exn_to_string e)
      |> Lwt.ignore_result

    method progress = progress
    method thread = t
    method iter (f : 'a -> unit) =
      Lwt_result.Infix.(
        t >>= (fun x -> f x; Lwt_result.return ())
        |> Lwt.ignore_result)
    method set_on_success x = _on_success <- x
    method set_on_error x = _on_error <- x

    method private _on_error e =
      (match _on_error with
       | None -> ()
       | Some f -> f (self :> 'a loader) e);
      let s = match error_prefix with
        | Some pfx -> Printf.sprintf "%s:\n %s" pfx e
        | None -> e in
      let error = Placeholder.Err.make ?icon:error_icon ~text:s () in
      super#append_child error;
      Lwt.return_unit

  end

(* TODO add loader to DOM only after certain timeout *)
class ['a] widget_loader ?text ?error_icon ?error_prefix
    ?(parent : #Dom_html.element Js.t option)
    (t : ((#Widget.t as 'a), string) Lwt_result.t) () =
  object(self)
    inherit ['a] loader ?text ?error_icon ?error_prefix t () as super

    method! init () : unit =
      super#init ();
      Lwt_result.Infix.(
        Lwt.async (fun () ->
            self#thread
            >>= fun (w : #Widget.t) ->
            (match parent with
             | Some p ->
               Element.append_child p w#root;
               Element.remove_child_safe p self#widget#root;
               w#layout ()
             | None ->
               self#append_child w;
               w#layout ());
            Lwt.return_ok ()));
      match parent with
      | None -> ()
      | Some p -> Element.append_child p self#widget#root

    method! destroy () : unit =
      super#destroy ();
      Lwt.async (fun () ->
          self#thread
          >>= function
          | Ok w -> w#destroy (); Lwt.return_ok ()
          | Error _ as e -> Lwt.return e)

  end

let create_loader ?text ?error_icon ?error_prefix ?on_error ?on_success t =
  new loader ?text ?error_icon ?error_prefix ?on_error ?on_success t ()

let create_widget_loader ?text ?error_icon ?error_prefix ?parent t =
  new widget_loader ?text ?error_icon ?error_prefix ?parent t ()
