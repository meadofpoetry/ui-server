open Containers
open Components

module Set = struct

  open Lwt.Infix

  let base_class = "mdc-apply-button"
  let busy_class = Markup.CSS.add_modifier base_class "busy"

  class t ?typ ?style ?icon ?dense ?compact ?ripple ?(label = "Применить")
          (signal : 'a option React.signal)
          (setter : ('a -> 'b Lwt.t)) () =
  object(self)

    val _loader = new Circular_progress.t ~size:25 ~indeterminate:true ()
    val mutable _click_listener = None

    inherit Button.t ?typ ?style ?icon ?dense
              ?compact ?ripple ~label () as super

    method! init () : unit =
      super#init ();
      self#add_class base_class;
      React.S.map (function Some _ -> self#set_disabled false
                          | None   -> self#set_disabled true)
        signal |> self#_keep_s;
      let listener =
        self#listen_click_lwt (fun _ _ ->
            match React.S.value signal with
            | Some v ->
               self#add_class busy_class;
               self#append_child _loader;
               Lwt.try_bind
                 (fun () -> setter v)
                 (fun _ -> self#finalize ())
                 (fun _ -> self#finalize ())
            | None -> Lwt.return_unit) in
      _click_listener <- Some listener

    method private finalize () : unit Lwt.t =
      self#remove_class busy_class;
      self#remove_child _loader;
      Lwt.return_unit

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

    inherit Button.t ?typ ?style ?icon ?dense ?compact ?ripple ~label () as super

    val _loader = new Circular_progress.t ~size:25 ()
    val! mutable _listener = None
    val mutable _getter = None
    val mutable _prev : 'a option = None
    val mutable _timer : Utils.interval_id option = None
    val mutable _timeout = timeout
    val _period = 250.

    method! init () : unit =
      super#init ();
      self#set_timeout timeout;
      self#add_class base_class;
      self#set_getter getter;
      match _getter with
      | None -> self#set_disabled true
      | Some _ -> self#set_disabled false

    method! destroy () : unit =
      super#destroy ();
      self#_stop_listen ()

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
      | None ->  _loader#set_indeterminate true
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
      | None -> ()
      | Some x ->
         Utils.clear_interval x;
         _timer <- None

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
                   Utils.set_interval (fun () ->
                       let cur = _loader#progress in
                       _loader#set_progress (cur +. _period))
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
  end

end
