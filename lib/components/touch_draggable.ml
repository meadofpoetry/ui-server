open Js_of_ocaml

let touch e = Js.Optdef.get (e##.changedTouches##item 0)
    (fun () -> failwith "touch fail")

let rec find_touch id num source =  (* given a touchList Js.t, finds a touch with needed identifier among *)
  if num < 0 then None
  else
  if Js.Optdef.test (source##item num)
  then let touch = Js.Optdef.get (source##item num)
           (fun () -> failwith "No touch with such id") in
    if touch##.identifier = id
    then Some touch
    else find_touch id (num - 1) source
  else find_touch id (num - 1) source

(* imitates an event of a given type, using touch and event target parameters. sets dataTransfer on demand *)
let dispatch ?data touch typ target : unit =
  let evt = Js.Unsafe.pure_js_expr "document.createEvent('Event')" in
  let coerced = Js.Unsafe.coerce evt in
  let optional = match typ with | "dragend" -> Js._false | _ -> Js._true in
  let () = coerced##initEvent (Js.string typ) Js._true optional in
  let () = Js.Unsafe.set evt "dataTransfer" (Js.Unsafe.pure_js_expr "new DataTransfer()") in
  coerced##.button := Js.string "0";
  coerced##.which := Js.string "1";
  coerced##.buttons := Js.string "1";
  coerced##.pageX := touch##.pageX;
  coerced##.pageY := touch##.pageY;
  coerced##.clientX := touch##.clientX;
  coerced##.clientY := touch##.clientY;
  coerced##.screenX := touch##.screenX;
  coerced##.screenY := touch##.screenY;
  (match data with
   | None -> ()
   | Some data ->
     let typ, data = data in
     coerced##.dataTransfer##setData typ data);
  (Js.Unsafe.coerce target)##dispatchEvent evt

(* finds element above the touched point, actually returns Dom_html.element Js.opt *)
let elt_from_point x y = (Js.Unsafe.coerce Dom_html.document)##elementFromPoint x y

class t ~data ~typ elt () = object(self)

  val mutable id = None
  val mutable drag = false
  val mutable clone = None
  val mutable delta = 0,0
  val mutable _start_listener = None
  val mutable _move_listener = None
  val mutable _end_listener = None

  method private handle_touch_end e _ =
    id <- None;
    drag <- false;
    let touch = touch e in
    Js.Opt.iter (elt_from_point touch##.clientX touch##.clientY)
      (fun x -> dispatch touch "drop" x ?data:(Some ((Js.string typ),data)));
    Utils.Option.iter (fun cln ->
        (try Dom.removeChild Dom_html.document##.body cln with _ -> ());
        clone <- None) clone;
    dispatch touch "dragend" elt;
    Utils.Option.iter Lwt.cancel _move_listener;
    Utils.Option.iter Lwt.cancel _end_listener;
    _move_listener <- None;
    _end_listener <- None;
    Lwt.return_unit

  method private handle_touch_move e _ =
    let touch = touch e in
    (match drag, id with
     | false , Some _ ->
       dispatch touch "dragstart" elt;
       drag  <- true;
       let cln = (Js.Unsafe.coerce elt)##cloneNode true in
       cln##.style##.width := Js.string @@ (string_of_int elt##.offsetWidth)^"px";
       cln##.style##.position := Js.string "absolute";
       cln##.style##.pointerEvents := Js.string "none";
       cln##.style##.opacity := Js.def @@ Js.string "0.5";
       cln##.style##.zIndex := Js.string "9999";
       clone <- Some cln;
       Dom.appendChild Dom_html.document##.body cln
     | true, Some id ->
       (match find_touch id (e##.changedTouches##.length-1)
                e##.changedTouches with
       | Some touch ->
         Js.Opt.iter (elt_from_point touch##.clientX touch##.clientY)
           (fun x -> dispatch touch "dragover" x
               ?data:(Some ((Js.string typ),data)));
         Utils.Option.iter (fun cln ->
             let dx, dy = delta in
             cln##.style##.left :=
               Js.string @@ (string_of_int (touch##.pageX - dx))^"px";
             cln##.style##.top  :=
               Js.string @@ (string_of_int (touch##.pageY - dy))^"px") clone
       | None       -> ())
     | _, _ -> ());
    Lwt.return_unit

  method private handle_touch_start e _ =
    Dom.preventDefault e;
    let touch = touch e in
    let rect = (Js.Unsafe.coerce elt)##getBoundingClientRect in
    let del_x, del_y = touch##.pageX - rect##.left, 0 in
    delta <- del_x, del_y;
    id <- Some touch##.identifier;
    _move_listener <- Some (Events.touchmoves Dom_html.window self#handle_touch_move);
    _end_listener <- Some (Events.touchends elt self#handle_touch_end);
    Lwt.return_unit

  initializer
    _start_listener <- Some (Events.touchstarts elt self#handle_touch_start)
end
