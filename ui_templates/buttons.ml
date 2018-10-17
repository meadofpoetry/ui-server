open Containers
open Components

module Set = struct

  open Lwt.Infix

  let base_class = "mdc-apply-button"
  let busy_class = Markup.CSS.add_modifier base_class "busy"

  class t ?typ ?style ?icon ?dense ?compact ?ripple ?(label = "Применить")
          (signal:'a option React.signal)
          (setter:('a -> 'b Lwt.t)) () =
  object(self)
    val _loader = new Circular_progress.t ~size:25 ~indeterminate:true ()
    val mutable _thread : unit Lwt.t option = None
    inherit Button.t ?typ ?style ?icon ?dense ?compact ?ripple ~label ()

    method private finalize () : unit Lwt.t =
      self#remove_class busy_class;
      self#remove_child _loader;
      Lwt.return_unit

    initializer
      React.S.map (function Some _ -> self#set_disabled false
                          | None   -> self#set_disabled true)
        signal |> self#_keep_s;
      Dom_events.listen self#root Dom_events.Typ.click (fun _ _ ->
          let is_finished = match _thread with
            | Some t -> (match Lwt.state t with
                         | Sleep -> false
                         | _     -> true)
            | None   -> true in
          (match is_finished, React.S.value signal with
           | true, Some v ->
              self#add_class busy_class;
              Dom.appendChild self#root _loader#root;
              Lwt.try_bind
                (fun () -> setter v)
                (fun _ -> self#finalize ())
                (fun _ -> self#finalize ())
              |> fun t -> _thread <- Some t;
           | _ -> ());
          true) |> ignore;
      self#add_class base_class
  end

end

module Get = struct

  open Lwt.Infix

  let base_class = "mdc-get-button"
  let busy_class = Markup.CSS.add_modifier base_class "busy"

  class ['a] t ?typ ?style ?icon ?dense ?compact ?ripple
          ?timeout ?(getter : (unit -> 'a Lwt.t) option)
          ~label () =
  object(self)

    inherit Button.t ?typ ?style ?icon ?dense ?compact ?ripple ~label ()

    val _loader = new Circular_progress.t ~size:25 ()
    val! mutable _listener = None
    val mutable _getter = None
    val mutable _prev : 'a option = None
    val mutable _timer : Dom_html.interval_id option = None
    val mutable _timeout = timeout
    val _period = 250.

    method progress = _loader

    method value : 'a option = _prev

    method getter = _getter
    method set_getter f =
      _getter <- f;
      match f with
      | None ->
         self#_stop_listen ();
         self#set_disabled true
      | Some f ->
         self#set_disabled false;
         self#_listen f

    method set_timeout (x : float option) =
      _timeout <- x;
      match x with
      | None ->
         _loader#set_indeterminate true
      | Some x ->
         _loader#set_max (x *. 1000.);
         _loader#set_indeterminate false

    (* Private methods *)

    method private _finalize () =
      _loader#set_progress _loader#max;
      Lwt_js.sleep (_period /. 1000.)
      >|= fun () ->
      self#remove_class busy_class;
      self#remove_child _loader;
      match _timer with
      | Some x ->
         Dom_html.window##clearInterval x;
         _timer <- None
      | None -> ()

    method private _stop_listen () = match _listener with
      | None -> ()
      | Some l -> Lwt.cancel l

    method private _listen f =
      self#_stop_listen ();
      self#listen_click_lwt ~cancel_handler:true (fun _ _ ->
          self#add_class busy_class;
          self#append_child _loader;
          Lwt.try_bind
            (fun () ->
              let t = f () in
              begin match _timeout with
              | None -> ()
              | Some _ ->
                 _loader#set_progress 0.;
                 let timer =
                   Dom_html.window##setInterval
                     (Js.wrap_callback (fun () ->
                          let cur = _loader#progress in
                          _loader#set_progress (cur +. _period)))
                     _period in
                 _timer <- Some timer;
              end;
              t)
            (fun v ->
              _prev <- Some v;
              self#_finalize ())
            (fun _ ->
              self#_finalize ()))
      |> fun t -> _listener <- Some t

    initializer
      self#set_timeout timeout;
      self#add_class base_class;
      self#set_getter getter;
      match _getter with
      | None -> self#set_disabled true
      | Some _ -> self#set_disabled false
  end

end
