open Containers
open Components
open Lwt.Infix

let base_class = "mdc-loader"

let timeout = 0.4

class ['a] loader
        ?(text : string option)
        ?(error_icon : #Widget.t option)
        ?(error_prefix : string option)
        ?(on_error : ('a loader -> string -> unit) option)
        ?(on_success : ('a loader -> 'a -> unit) option)
        (t : ('a,string) Lwt_result.t)
        () =
object(self)
  val pgs = Placeholder.create_progress ?text ()
  val mutable _on_success = on_success
  val mutable _on_error   = on_error
  inherit Widget.t Dom_html.(createDiv document) ()

  method progress = pgs
  method thread   = t
  method iter f   =
    Lwt_result.Infix.(
      t >>= (fun x -> f x; Lwt_result.return ())
      |> Lwt.ignore_result)
  method set_on_success x = _on_success <- x
  method set_on_error   x = _on_error   <- x

  method private _on_error e =
    self#remove_child pgs;
    Option.iter (fun f -> f (self :> 'a loader) e) _on_error;
    let s = match error_prefix with
      | Some pfx -> Printf.sprintf "%s:\n %s" pfx e
      | None -> e in
    let error = Placeholder.create_with_error ?icon:error_icon ~text:s () in
    self#append_child error;
    Lwt.return_unit

  initializer
    self#add_class base_class;
    self#append_child pgs;
    Lwt.try_bind
      (fun () -> t)
      (fun r ->
        self#remove_child pgs;
        match r with
        | Ok x    ->
           Option.iter (fun f -> f (self :> 'a loader) x) _on_success;
           Lwt.return_unit
        | Error e -> self#_on_error e)
      (fun e -> self#_on_error @@ Printexc.to_string e)
    |> Lwt.ignore_result

end

class ['a] widget_loader ?text ?error_icon ?error_prefix
        ?(parent : #Widget.t option)
        (t : ((#Widget.t as 'a), string) Lwt_result.t) () =
object(self)
  inherit ['a] loader ?text ?error_icon ?error_prefix t () as super

  initializer
    Lwt_result.Infix.(
    self#thread
    >|= (fun (w : #Widget.t) ->
      (match parent with
       | Some p -> p#append_child w;
                   p#remove_child (self :> Widget.t)
       | None -> self#append_child w)))
    |> Lwt.ignore_result;
    Option.iter (fun (p : #Widget.t) ->
        p#append_child (self :> Widget.t)) parent
end

let create_loader ?text ?error_icon ?error_prefix ?on_error ?on_success t =
  new loader ?text ?error_icon ?error_prefix ?on_error ?on_success t ()

let create_widget_loader ?text ?error_icon ?error_prefix ?parent t =
  new widget_loader ?text ?error_icon ?error_prefix ?parent t ()
