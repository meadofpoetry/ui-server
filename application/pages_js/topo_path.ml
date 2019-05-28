open Js_of_ocaml
open Js_of_ocaml_tyxml
open Application_types
open Components
open Topo_types
open Lwt.Infix

let get_output_point (elt : #Dom_html.element Js.t) : point =
  { x = elt##.offsetLeft + elt##.offsetWidth
  ; y = elt##.offsetTop + (elt##.offsetHeight / 2)
  }

let get_input_point ~num i (elt : #Dom_html.element Js.t) : point =
  let h = elt##.offsetHeight / num in
  let x = elt##.offsetLeft in
  let y = elt##.offsetTop + (i * h) + (h / 2) in
  { x; y }

class switch (port : Topology.topo_port) setter () =
  let _class = "topology__switch" in
  let signal, push = React.S.create ~eq:(=) false in
  let elt = Tyxml_js.To_dom.of_element @@ Switch.Markup.create () in
  object(self)

    inherit Switch.t elt () as super

    val mutable _state = `Unavailable

    method! init () : unit =
      super#init ();
      super#set_disabled true;
      super#add_class _class

    method port : Topology.topo_port = port

    method s_changing : bool React.signal = signal

    method set_state (x : connection_state) : unit =
      _state <- x;
      match x with
      | `Active | `Sync | `Sync_lost ->
         super#toggle ~force:true ();
         super#set_disabled false
      | `Muted ->
         super#set_disabled false;
         super#toggle ~force:false ()
      | `Unavailable ->
         super#set_disabled true

    method set_changing : bool -> unit = function
      | true -> super#set_disabled true
      | false ->
         match _state with
         | `Unavailable -> ()
         | _ -> super#set_disabled false

    method! private notify_change () =
      push true;
      setter port.port super#checked
      >>= function
      | Ok _ -> push false; Lwt.return_unit
      | Error _ ->
        push false;
        (* return current state back *)
        self#set_state _state;
        Lwt.return_unit

  end

let _class = "topology__path"
let active_class = BEM.add_modifier _class "active"
let muted_class = BEM.add_modifier _class "muted"
let sync_class = BEM.add_modifier _class "sync"
let no_sync_class = BEM.add_modifier _class "no-sync"

class t ~(left_node : node_entry)
        ~(right_point : connection_point)
        ~(f_lp : unit -> point)
        ~(f_rp : unit -> point)
        ~port_setter
        () =
  let switch = match right_point with
    | `Iface _ -> None
    | `Port p  ->
       if not p.switchable then None
       else Some (new switch p port_setter ()) in
  let elt = Tyxml_js.Svg.(
      path ~a:([ a_fill `None
               ; a_stroke (`Color ("white",None))
               ; a_stroke_width (2., None)])[]
      |> toelt) |> Js.Unsafe.coerce in
  object(self)

    inherit Widget.t elt () as super

    val mutable state : connection_state = `Muted

    method! init () : unit =
      super#init ();
      super#add_class _class;
      self#set_state state

    method left_node : node_entry = left_node
    method switch : switch option = switch
    method set_state (x : connection_state) : unit =
      state <- x;
      Utils.Option.iter (fun sw -> sw#set_state x) switch;
      match state with
      | `Muted | `Unavailable ->
         super#add_class muted_class;
         super#remove_class active_class;
         super#remove_class sync_class;
         super#remove_class no_sync_class;
      | `Active ->
         super#add_class active_class;
         super#remove_class muted_class;
         super#remove_class sync_class;
         super#remove_class no_sync_class;
      | `Sync ->
         super#add_class sync_class;
         super#remove_class active_class;
         super#remove_class muted_class;
         super#remove_class no_sync_class;
      | `Sync_lost ->
         super#add_class no_sync_class;
         super#remove_class active_class;
         super#remove_class muted_class;
         super#remove_class sync_class

    method! layout () : unit =
      super#layout ();
      let step = 80 in
      let left = f_lp () in
      let right = f_rp () in
      let top, height =
        if left.y > right.y
        then left.y, left.y  - right.y
        else right.y, right.y - left.y in
      Utils.Option.iter (fun sw ->
          let x = right.x + 15 in
          let y = right.y - (sw#root##.offsetHeight / 2) in
          sw#root##.style##.top := Utils.px_js y;
          sw#root##.style##.left := Utils.px_js x) switch;
      let width = right.x - left.x in
      let path =
        if abs (left.y - right.y) < 4
        then Printf.sprintf "M %d %d L %d %d" left.x left.y right.x left.y
        else
          if right.x - left.x < step
          then
            self#_make_straight_path
              left.x left.y
              left.x left.y (left.x + width / 2) left.y
              (left.x + width / 2) (top - height / 2)
              (left.x + width / 2) (top - height / 2)
              (left.x + width / 2) right.y right.x right.y
          else
            self#_make_curved_path
              left.x left.y
              (right.x - step) left.y
              (right.x - step) left.y (right.x - step / 2) left.y
              (right.x - step / 2) (top - height / 2)
              (right.x - step / 2) (top - height / 2)
              (right.x - step / 2) (right.y) right.x right.y in
      self#set_attribute "d" path

    method private _make_straight_path =
      Printf.sprintf "M %d %d C %d %d %d %d %d %d C %d %d %d %d %d %d"

    method private _make_curved_path =
      Printf.sprintf "M %d %d L %d %d C %d %d %d %d %d %d C %d %d %d %d %d %d"
  end
