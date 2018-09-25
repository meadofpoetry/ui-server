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
  let _  = React.S.map (function Some _ -> b#set_disabled false
                               | None   -> b#set_disabled true) s in
  let _  = React.E.map (fun _ -> Option.iter (fun s -> f s |> ignore)
                                 @@ React.S.value s) b#e_click in
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
          ?(getter:(unit -> 'a Lwt.t) option) () =
  object(self)
    val _loader = new Circular_progress.t ~size:25 ~indeterminate:true ()
    val mutable _getter = getter
    val mutable _prev   : 'a option = None
    val mutable _thread : unit Lwt.t option = None
    inherit Button.t ?typ ?style ?icon ?dense ?compact ?ripple ~label ()

    method value : 'a option = _prev

    method getter = _getter
    method set_getter f =
      self#set_disabled @@ Option.is_none f;
      _getter <- f

    initializer
      (match _getter with None   -> self#set_disabled true
                        | Some _ -> self#set_disabled false);
      Dom_events.listen self#root Dom_events.Typ.click (fun _ _ ->
          let is_finished = match _thread with
            | Some t -> (match Lwt.state t with
                         | Return _ -> true
                         | Fail _   -> true
                         | Sleep    -> false)
            | None   -> true in
          (match is_finished, _getter with
           | true, Some f ->
              self#add_class busy_class;
              Dom.appendChild self#root _loader#root;
              f ()
              >|= (fun v ->
                _prev <- Some v;
                self#remove_class busy_class;
                Dom.removeChild self#root _loader#root)
              |> fun t -> _thread <- Some t
           | _ -> ());
          true) |> ignore;
      self#add_class base_class
  end

end
