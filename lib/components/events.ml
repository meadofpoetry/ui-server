open Js_of_ocaml
open Js_of_ocaml_lwt

let ( >>= ) = Lwt.bind

let passive_supported = ref false

let check_passive () =
  let open Js.Unsafe in
  let options = global##._Object##defineProperty
      (obj [||])
      (Js.string "passive")
      (obj [|"get", inject (Js.wrap_callback (fun _ ->
           passive_supported := true))|]) in
  let listener =
    Dom_html.addEventListener
      Dom_html.window
      (Dom_events.Typ.make "test")
      (Dom_html.handler (fun _ -> Js._true))
      (Js.Unsafe.coerce options) in
  Dom_html.removeEventListener listener

let () = check_passive ()

module Typ = struct

  open Dom_events.Typ

  type delta_mode =
    | DOM_DELTA_PIXEL
    | DOM_DELTA_LINE
    | DOM_DELTA_PAGE

  class type wheelEvent =
    object
      inherit Dom_html.mouseEvent
      method deltaX : int Js.readonly_prop
      method deltaY : int Js.readonly_prop
      method deltaZ : int Js.readonly_prop
      method deltaMode : delta_mode Js.readonly_prop
    end

  (** https://www.w3.org/TR/pointerevents/#dom-pointerevent *)
  class type pointerEvent =
    object
      inherit Dom_html.mouseEvent
      method pointerId : int Js.readonly_prop
      method width : float Js.readonly_prop
      method height : float Js.readonly_prop
      method pressure : float Js.readonly_prop
      method tangentialPressure : float Js.readonly_prop
      method tiltX : int Js.readonly_prop
      method tiltY : int Js.readonly_prop
      method twist : int Js.readonly_prop
      method pointerType : Js.js_string Js.t Js.readonly_prop
      method isPrimary : bool Js.t Js.readonly_prop
    end

  let (wheel : wheelEvent Js.t typ) =
    make "wheel"

  let (pointerdown : pointerEvent Js.t typ) =
    make "pointerdown"

  let (focusin : Dom_html.event Js.t typ) =
    make "focusin"

  let (focusout : Dom_html.event Js.t typ) =
    make "focusout"

end

include Lwt_js_events

let make_event event_kind ?(use_capture = false) ?(passive = false) target =
  let el = ref Js.null in
  let t, w = Lwt.task () in
  let cancel () = Js.Opt.iter !el Dom_html.removeEventListener in
  Lwt.on_cancel t cancel;
  let options =
    if !passive_supported
    then Js.Unsafe.coerce
        (object%js
          val capture = Js.bool use_capture
          val passive = Js.bool passive
        end)
    else Js.bool use_capture in
  el :=
    Js.some
      (Dom.addEventListener
         target
         event_kind
         (Dom_html.handler (fun (ev : #Dom_html.event Js.t) ->
              cancel ();
              Lwt.wakeup w ev;
              Js.bool true))
         options);
  t

let catch_cancel f x =
  Lwt.catch
    (fun () -> f x)
    (function
      | Lwt.Canceled -> Lwt.return ()
      | e -> Lwt.fail e)

let with_error_log f x =
  Lwt.catch
    (fun () -> f x)
    (fun e ->
      Firebug.console##log (Js.string (Printexc.to_string e));
      Lwt.return ())

let seq_loop evh ?(cancel_handler = false) ?use_capture ?passive target handler =
  let cancelled = ref false in
  let cur = ref (Lwt.fail (Failure "Lwt_js_event")) in
  (* Using Lwt.fail as default, to be polymorphic *)
  let cur_handler = ref (Lwt.return ()) in
  let lt, _lw = Lwt.task () in
  Lwt.on_cancel lt (fun () ->
      Lwt.cancel !cur;
      if cancel_handler then Lwt.cancel !cur_handler;
      cancelled := true);
  let rec aux () =
    if not !cancelled
    (* In the case it has been cancelled
                      during the previous handler,
                      we do not reinstall the event handler *)
    then (
      let t = evh ?use_capture ?passive target in
      cur := t;
      t
      >>= fun e ->
      cur_handler := with_error_log (handler e) lt;
      !cur_handler >>= aux)
    else Lwt.return ()
  in
  Lwt.async (catch_cancel aux);
  lt

let click ?use_capture ?passive target = make_event Dom_html.Event.click ?use_capture ?passive target

let dblclick ?use_capture ?passive target = make_event Dom_html.Event.dblclick ?use_capture ?passive target

let mousedown ?use_capture ?passive target =
  make_event Dom_html.Event.mousedown ?use_capture ?passive target

let mouseup ?use_capture ?passive target = make_event Dom_html.Event.mouseup ?use_capture ?passive target

let mouseover ?use_capture ?passive target =
  make_event Dom_html.Event.mouseover ?use_capture ?passive target

let mousemove ?use_capture ?passive target =
  make_event Dom_html.Event.mousemove ?use_capture ?passive target

let mouseout ?use_capture ?passive target = make_event Dom_html.Event.mouseout ?use_capture ?passive target

let keypress ?use_capture ?passive target = make_event Dom_html.Event.keypress ?use_capture ?passive target

let keydown ?use_capture ?passive target = make_event Dom_html.Event.keydown ?use_capture ?passive target

let keyup ?use_capture ?passive target = make_event Dom_html.Event.keyup ?use_capture ?passive target

let change ?use_capture ?passive target = make_event Dom_html.Event.change ?use_capture ?passive target

let input ?use_capture ?passive target = make_event Dom_html.Event.input ?use_capture ?passive target

let timeupdate ?use_capture ?passive target =
  make_event Dom_html.Event.timeupdate ?use_capture ?passive target

let dragstart ?use_capture ?passive target =
  make_event Dom_html.Event.dragstart ?use_capture ?passive target

let dragend ?use_capture ?passive target = make_event Dom_html.Event.dragend ?use_capture ?passive target

let dragenter ?use_capture ?passive target =
  make_event Dom_html.Event.dragenter ?use_capture ?passive target

let dragover ?use_capture ?passive target = make_event Dom_html.Event.dragover ?use_capture ?passive target

let dragleave ?use_capture ?passive target =
  make_event Dom_html.Event.dragleave ?use_capture ?passive target

let drag ?use_capture ?passive target = make_event Dom_html.Event.drag ?use_capture ?passive target

let drop ?use_capture ?passive target = make_event Dom_html.Event.drop ?use_capture ?passive target

let focus ?use_capture ?passive target = make_event Dom_html.Event.focus ?use_capture ?passive target

let blur ?use_capture ?passive target = make_event Dom_html.Event.blur ?use_capture ?passive target

let scroll ?use_capture ?passive target = make_event Dom_html.Event.scroll ?use_capture ?passive target

let submit ?use_capture ?passive target = make_event Dom_html.Event.submit ?use_capture ?passive target

let select ?use_capture ?passive target = make_event Dom_html.Event.select ?use_capture ?passive target

let abort ?use_capture ?passive target = make_event Dom_html.Event.abort ?use_capture ?passive target

let error ?use_capture ?passive target = make_event Dom_html.Event.error ?use_capture ?passive target

let load ?use_capture ?passive target = make_event Dom_html.Event.load ?use_capture ?passive target

let canplay ?use_capture ?passive target = make_event Dom_html.Event.canplay ?use_capture ?passive target

let canplaythrough ?use_capture ?passive target =
  make_event Dom_html.Event.canplaythrough ?use_capture ?passive target

let durationchange ?use_capture ?passive target =
  make_event Dom_html.Event.durationchange ?use_capture ?passive target

let emptied ?use_capture ?passive target = make_event Dom_html.Event.emptied ?use_capture ?passive target

let ended ?use_capture ?passive target = make_event Dom_html.Event.ended ?use_capture ?passive target

let loadeddata ?use_capture ?passive target =
  make_event Dom_html.Event.loadeddata ?use_capture ?passive target

let loadedmetadata ?use_capture ?passive target =
  make_event Dom_html.Event.loadedmetadata ?use_capture ?passive target

let loadstart ?use_capture ?passive target =
  make_event Dom_html.Event.loadstart ?use_capture ?passive target

let pause ?use_capture ?passive target = make_event Dom_html.Event.pause ?use_capture ?passive target

let play ?use_capture ?passive target = make_event Dom_html.Event.play ?use_capture ?passive target

let playing ?use_capture ?passive target = make_event Dom_html.Event.playing ?use_capture ?passive target

let ratechange ?use_capture ?passive target =
  make_event Dom_html.Event.ratechange ?use_capture ?passive target

let seeked ?use_capture ?passive target = make_event Dom_html.Event.seeked ?use_capture ?passive target

let seeking ?use_capture ?passive target = make_event Dom_html.Event.seeking ?use_capture ?passive target

let stalled ?use_capture ?passive target = make_event Dom_html.Event.stalled ?use_capture ?passive target

let suspend ?use_capture ?passive target = make_event Dom_html.Event.suspend ?use_capture ?passive target

let volumechange ?use_capture ?passive target =
  make_event Dom_html.Event.volumechange ?use_capture ?passive target

let waiting ?use_capture ?passive target = make_event Dom_html.Event.waiting ?use_capture ?passive target

let touchstart ?use_capture ?passive target =
  make_event Dom_html.Event.touchstart ?use_capture ?passive target

let touchmove ?use_capture ?passive target =
  make_event Dom_html.Event.touchmove ?use_capture ?passive target

let touchend ?use_capture ?passive target = make_event Dom_html.Event.touchend ?use_capture ?passive target

let touchcancel ?use_capture ?passive target =
  make_event Dom_html.Event.touchcancel ?use_capture ?passive target

let clicks ?cancel_handler ?use_capture ?passive t = seq_loop click ?cancel_handler ?use_capture ?passive t

let dblclicks ?cancel_handler ?use_capture ?passive t =
  seq_loop dblclick ?cancel_handler ?use_capture ?passive t

let mousedowns ?cancel_handler ?use_capture ?passive t =
  seq_loop mousedown ?cancel_handler ?use_capture ?passive t

let mouseups ?cancel_handler ?use_capture ?passive t =
  seq_loop mouseup ?cancel_handler ?use_capture ?passive t

let mouseovers ?cancel_handler ?use_capture ?passive t =
  seq_loop mouseover ?cancel_handler ?use_capture ?passive t

let mousemoves ?cancel_handler ?use_capture ?passive t =
  seq_loop mousemove ?cancel_handler ?use_capture ?passive t

let mouseouts ?cancel_handler ?use_capture ?passive t =
  seq_loop mouseout ?cancel_handler ?use_capture ?passive t

let keypresses ?cancel_handler ?use_capture ?passive t =
  seq_loop keypress ?cancel_handler ?use_capture ?passive t

let keydowns ?cancel_handler ?use_capture ?passive t =
  seq_loop keydown ?cancel_handler ?use_capture ?passive t

let keyups ?cancel_handler ?use_capture ?passive t = seq_loop keyup ?cancel_handler ?use_capture ?passive t

let changes ?cancel_handler ?use_capture ?passive t =
  seq_loop change ?cancel_handler ?use_capture ?passive t

let inputs ?cancel_handler ?use_capture ?passive t = seq_loop input ?cancel_handler ?use_capture ?passive t

let timeupdates ?cancel_handler ?use_capture ?passive t =
  seq_loop timeupdate ?cancel_handler ?use_capture ?passive t

let dragstarts ?cancel_handler ?use_capture ?passive t =
  seq_loop dragstart ?cancel_handler ?use_capture ?passive t

let dragends ?cancel_handler ?use_capture ?passive t =
  seq_loop dragend ?cancel_handler ?use_capture ?passive t

let dragenters ?cancel_handler ?use_capture ?passive t =
  seq_loop dragenter ?cancel_handler ?use_capture ?passive t

let dragovers ?cancel_handler ?use_capture ?passive t =
  seq_loop dragover ?cancel_handler ?use_capture ?passive t

let dragleaves ?cancel_handler ?use_capture ?passive t =
  seq_loop dragleave ?cancel_handler ?use_capture ?passive t

let drags ?cancel_handler ?use_capture ?passive t = seq_loop drag ?cancel_handler ?use_capture ?passive t

let drops ?cancel_handler ?use_capture ?passive t = seq_loop drop ?cancel_handler ?use_capture ?passive t

let touchstarts ?cancel_handler ?use_capture ?passive t =
  seq_loop touchstart ?cancel_handler ?use_capture ?passive t

let touchmoves ?cancel_handler ?use_capture ?passive t =
  seq_loop touchmove ?cancel_handler ?use_capture ?passive t

let touchends ?cancel_handler ?use_capture ?passive t =
  seq_loop touchend ?cancel_handler ?use_capture ?passive t

let touchcancels ?cancel_handler ?use_capture ?passive t =
  seq_loop touchcancel ?cancel_handler ?use_capture ?passive t

let focuses ?cancel_handler ?use_capture ?passive t =
  seq_loop focus ?cancel_handler ?use_capture ?passive t

let blurs ?cancel_handler ?use_capture ?passive t = seq_loop blur ?cancel_handler ?use_capture ?passive t

let scrolls ?cancel_handler ?use_capture ?passive t =
  seq_loop scroll ?cancel_handler ?use_capture ?passive t

let submits ?cancel_handler ?use_capture ?passive t =
  seq_loop submit ?cancel_handler ?use_capture ?passive t

let selects ?cancel_handler ?use_capture ?passive t =
  seq_loop select ?cancel_handler ?use_capture ?passive t

let aborts ?cancel_handler ?use_capture ?passive t = seq_loop abort ?cancel_handler ?use_capture ?passive t

let errors ?cancel_handler ?use_capture ?passive t = seq_loop error ?cancel_handler ?use_capture ?passive t

let loads ?cancel_handler ?use_capture ?passive t = seq_loop load ?cancel_handler ?use_capture ?passive t

let canplays ?cancel_handler ?use_capture ?passive t =
  seq_loop canplay ?cancel_handler ?use_capture ?passive t

let canplaythroughs ?cancel_handler ?use_capture ?passive t =
  seq_loop canplaythrough ?cancel_handler ?use_capture ?passive t

let durationchanges ?cancel_handler ?use_capture ?passive t =
  seq_loop durationchange ?cancel_handler ?use_capture ?passive t

let emptieds ?cancel_handler ?use_capture ?passive t =
  seq_loop emptied ?cancel_handler ?use_capture ?passive t

let endeds ?cancel_handler ?use_capture ?passive t = seq_loop ended ?cancel_handler ?use_capture ?passive t

let loadeddatas ?cancel_handler ?use_capture ?passive t =
  seq_loop loadeddata ?cancel_handler ?use_capture ?passive t

let loadedmetadatas ?cancel_handler ?use_capture ?passive t =
  seq_loop loadedmetadata ?cancel_handler ?use_capture ?passive t

let loadstarts ?cancel_handler ?use_capture ?passive t =
  seq_loop loadstart ?cancel_handler ?use_capture ?passive t

let pauses ?cancel_handler ?use_capture ?passive t = seq_loop pause ?cancel_handler ?use_capture ?passive t

let plays ?cancel_handler ?use_capture ?passive t = seq_loop play ?cancel_handler ?use_capture ?passive t

let playings ?cancel_handler ?use_capture ?passive t =
  seq_loop playing ?cancel_handler ?use_capture ?passive t

let ratechanges ?cancel_handler ?use_capture ?passive t =
  seq_loop ratechange ?cancel_handler ?use_capture ?passive t

let seekeds ?cancel_handler ?use_capture ?passive t =
  seq_loop seeked ?cancel_handler ?use_capture ?passive t

let seekings ?cancel_handler ?use_capture ?passive t =
  seq_loop seeking ?cancel_handler ?use_capture ?passive t

let stalleds ?cancel_handler ?use_capture ?passive t =
  seq_loop stalled ?cancel_handler ?use_capture ?passive t

let suspends ?cancel_handler ?use_capture ?passive t =
  seq_loop suspend ?cancel_handler ?use_capture ?passive t

let volumechanges ?cancel_handler ?use_capture ?passive t =
  seq_loop volumechange ?cancel_handler ?use_capture ?passive t

let waitings ?cancel_handler ?use_capture ?passive t =
  seq_loop waiting ?cancel_handler ?use_capture ?passive t

let wheel ?use_capture ?passive target : Typ.wheelEvent Js.t Lwt.t =
  make_event Typ.wheel ?use_capture ?passive target

let wheels ?cancel_handler ?use_capture ?passive t =
  seq_loop wheel ?cancel_handler ?use_capture ?passive t

let pointerdown ?use_capture ?passive target =
  make_event Typ.pointerdown ?use_capture ?passive target

let pointerdowns ?cancel_handler ?use_capture ?passive t =
  seq_loop pointerdown ?cancel_handler ?use_capture ?passive t

let focusin ?use_capture ?passive target =
  make_event Typ.focusin ?use_capture ?passive target

let focusins ?cancel_handler ?use_capture ?passive t =
  seq_loop focusin ?cancel_handler ?use_capture ?passive t

let focusout ?use_capture ?passive target =
  make_event Typ.focusout ?use_capture ?passive target

let focusouts ?cancel_handler ?use_capture ?passive t =
  seq_loop focusout ?cancel_handler ?use_capture ?passive t
