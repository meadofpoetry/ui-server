open Containers
open Components

(* FIXME legacy, remove *)
(** Creates apply button which is disabled when signal value is None.
    Sends provided request when clicked *)
let create_apply : type a b. (a option React.signal) -> (a -> (b,_) Lwt_result.t) -> Button.t =
  fun s f ->
  let base_class = "mdc-apply-button" in
  let b  = new Button.t ~label:"Применить" () in
  let () = b#add_class base_class in
  (* FIXME store signal *)
  let _ = React.S.map (function Some _ -> b#set_disabled false
                              | None -> b#set_disabled true) s in
  b#listen_click_lwt (fun _ _ ->
      Option.iter (fun s -> f s |> ignore) @@ React.S.value s;
      Lwt.return_unit) |> Lwt.ignore_result;
  b

module Set = struct

  open Lwt.Infix

  let base_class = "mdc-apply-button"
  let busy_class = Markup.CSS.add_modifier base_class "busy"

  class t ?typ ?style ?icon ?dense ?compact ?ripple ?(label="Применить")
          (signal:'a option React.signal)
          (setter:('a -> 'b Lwt.t)) () =
  object(self)
    val _loader = new Circular_progress.t ~size:25 ~indeterminate:true ()
    val mutable _thread : unit Lwt.t option = None
    inherit Button.t ?typ ?style ?icon ?dense ?compact ?ripple ~label ()
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
              setter v
              >|= (fun _ -> self#remove_class busy_class;
                            Dom.removeChild self#root _loader#root)
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

  class ['a] t ?typ ?style ?icon ?dense ?compact ?ripple ~label
          ?(getter : (unit -> 'a Lwt.t) option) () =
  object(self)

    inherit Button.t ?typ ?style ?icon ?dense ?compact ?ripple ~label ()

    val _loader = new Circular_progress.t ~size:25 ~indeterminate:true ()
    val! mutable _listener = None
    val mutable _getter = None
    val mutable _prev : 'a option = None

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

    (* Private methods *)

    method private _stop_listen () = match _listener with
      | None -> ()
      | Some l -> Lwt.cancel l

    method private _listen f =
      self#_stop_listen ();
      self#listen_click_lwt ~cancel_handler:true (fun _ _ ->
          self#add_class busy_class;
          self#append_child _loader;
          f ()
          >|= (fun v ->
            _prev <- Some v;
            self#remove_class busy_class;
            self#remove_child _loader))
      |> fun t -> _listener <- Some t

    initializer
      self#add_class base_class;
      self#set_getter getter;
      match _getter with
      | None -> self#set_disabled true
      | Some _ -> self#set_disabled false
  end

end
