open Js_of_ocaml
open Js_of_ocaml_tyxml
open Utils

include Components_tyxml.Switch
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

let ( >>= ) = Lwt.bind

class t ?on_change (elt : #Dom_html.element Js.t) () =
object(self)
  val input_elt : Dom_html.inputElement Js.t =
    find_element_by_class_exn elt CSS.native_control
  val mutable _ripple : Ripple.t option = None
  val mutable _change_listener = None

  inherit Widget.t elt () as super

  method! init () : unit =
    super#init ();
    _ripple <- Some (self#create_ripple ())

  method! initial_sync_with_dom () : unit =
    super#initial_sync_with_dom ();
    let change_listener =
      Events.listen_lwt input_elt Events.Typ.change (fun _ _ ->
          super#toggle_class ~force:self#checked CSS.checked;
          self#notify_change ()) in
    _change_listener <- Some change_listener;
    input_elt##.checked := input_elt##.checked

  method! layout () : unit =
    super#layout ();
    match _ripple with
    | None -> ()
    | Some r -> Ripple.layout r

  method! destroy () : unit =
    super#destroy ();
    (* Destroy internal components *)
    Utils.Option.iter Ripple.destroy _ripple;
    _ripple <- None;
    (* Detach event listeners *)
    Utils.Option.iter Lwt.cancel _change_listener;
    _change_listener <- None

  method disabled : bool =
    Js.to_bool input_elt##.disabled

  method set_disabled (x : bool) : unit =
    input_elt##.disabled := Js.bool x;
    super#toggle_class ~force:x CSS.disabled

  method checked : bool =
    Js.to_bool input_elt##.checked

  method toggle ?(notify = false) ?(force : bool option) () : unit =
    let v = match force with None -> not self#checked | Some x -> x in
    input_elt##.checked := Js.bool v;
    super#toggle_class ~force:v CSS.checked;
    if notify then Lwt.async self#notify_change

  method input_element : Dom_html.inputElement Js.t =
    input_elt

  method ripple : Ripple.t option =
    _ripple

  (* Private methods *)

  method private notify_change () : unit Lwt.t =
    match on_change with
    | None -> Lwt.return_unit
    | Some f -> f (self :> t)

  method private create_ripple () : Ripple.t =
    let selector = "." ^ CSS.thumb_underlay in
    let (surface : Dom_html.element Js.t) =
      Js.Opt.get (elt##querySelector (Js.string selector))
        (fun () -> failwith "no ripple surface element found")in
    Ripple.attach ~unbounded:true surface
end

let make ?input_id ?checked ?disabled ?on_change () : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?input_id ?checked ?disabled () in
  new t ?on_change elt ()

let attach ?on_change (elt : #Dom_html.element Js.t) : t =
  new t ?on_change elt ()
