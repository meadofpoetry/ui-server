open Containers
open Components
open Lwt.Infix

class ['a] loader
           ?(text:         string option)
           ?(error_icon:   string option)
           ?(error_prefix: string option)
           ?(on_error:     ('a loader -> string -> unit) option)
           ?(on_success:   ('a loader -> 'a     -> unit) option)
           (t:             ('a,string) Lwt_result.t)
           () =
object(self)
  val pgs = Placeholder.create_progress ?text ()
  val mutable _on_success = on_success
  val mutable _on_error   = on_error
  inherit Widget.t (Dom_html.createDiv Dom_html.document) ()

  method progress = pgs
  method thread   = t
  method iter f   = Lwt_result.Infix.(t >>= (fun x -> f x; Lwt_result.return ()) |> Lwt.ignore_result)
  method set_on_success x = _on_success <- x
  method set_on_error   x = _on_error   <- x

  method private _on_error e =
    (try Dom.removeChild self#root pgs#root with _ -> ());
    Option.iter (fun f -> f (self :> 'a loader) e) _on_error;
    let s = (match error_prefix with
             | Some pfx -> Printf.sprintf "%s:\n %s" pfx e
             | None     -> e)
    in
    let error = Placeholder.create_with_error ?icon:error_icon ~text:s () in
    Dom.appendChild self#root error#root;
    Lwt.return_unit

  initializer
    Lwt.try_bind
      (fun () -> Dom.appendChild self#root pgs#root; t)
      (fun r ->
        (try Dom.removeChild self#root pgs#root with _ -> ());
        match r with
        | Ok x    ->
           Option.iter (fun f -> f (self :> 'a loader) x) _on_success;
           Lwt.return_unit
        | Error e -> self#_on_error e)
      (fun e -> self#_on_error @@ Printexc.to_string e)
    |> Lwt.ignore_result

end

class widget_loader ?text ?error_icon ?error_prefix ?(parent:#Widget.t option)
        (t:(Widget.t,string) Lwt_result.t) () =
object(self)
  inherit [Widget.t] loader ?text ?error_icon ?error_prefix t () as super
  initializer
    Lwt_result.Infix.(
    t >>= (fun w -> (match parent with
                     | Some p -> Dom.appendChild p#root w#root;
                                 (try Dom.removeChild p#root self#root with _ -> ())
                     | None   -> Dom.appendChild self#root w#root);
                    Lwt_result.return ()))
    |> Lwt.ignore_result;
    Option.iter (fun p -> Dom.appendChild p#root self#root) parent
end

let create_loader ?text ?error_icon ?error_prefix ?on_error ?on_success t =
  new loader ?text ?error_icon ?error_prefix ?on_error ?on_success t ()

let create_widget_loader ?text ?error_icon ?error_prefix ?parent t =
  new widget_loader ?text ?error_icon ?error_prefix ?parent t ()

