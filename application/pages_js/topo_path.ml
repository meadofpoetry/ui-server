open Js_of_ocaml
open Js_of_ocaml_tyxml
open Application_types
open Components
open Topo_types

let ( >>= ) = Lwt.( >>= )

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
  let s_forbidden, set_forbidden = React.S.create ~eq:(=) false in
  let elt = Tyxml_js.To_dom.of_element @@ Switch.Markup.create () in
  object(self)

    inherit Switch.t elt () as super

    val mutable _changing = false
    val mutable _state = `Unavailable

    method! init () : unit =
      super#init ();
      super#set_disabled true;
      super#add_class _class

    method port : Topology.topo_port = port

    method forbidden : bool React.signal = s_forbidden

    method set_state (x : connection_state) : unit =
      _state <- x;
      if not _changing then match x with
        | `Active | `Sync | `Sync_lost ->
          super#toggle ~notify:false ~force:true ();
          super#set_disabled false
        | `Muted ->
          super#toggle ~notify:false ~force:false ();
          super#set_disabled false
        | `Unavailable ->
          super#set_disabled true

    method set_forbidden x =
      _changing <- x;
      match x, _state with
      | true, _ -> super#set_disabled true
      | false, `Unavailable -> ()
      | false, _ -> self#set_disabled false

    (** Called when a user clicks on a switch *)
    method! private notify_change () =
      set_forbidden true;
      setter port.port super#checked
      >>= fun _ -> set_forbidden false; Lwt.return_unit

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
        let offset = step / 4 in
        if right.x - left.x < step
        then
          self#_make_single_path
            left.x left.y
            (left.x + width / 2 + offset) left.y
            (left.x + width / 2) (top - height / 2)
            (left.x + width / 2 - offset) right.y
            right.x right.y
        else
          self#_make_complex_path
            left.x left.y
            (right.x - step) left.y
            (right.x - step / 2 + offset) left.y
            (right.x - step / 2) (top - height / 2)
            (right.x - step / 2 - offset) (right.y)
            right.x right.y in
      self#set_attribute "d" path

    method private _make_single_path =
      Printf.sprintf "M %d %d Q %d %d, %d %d Q %d %d, %d %d"

    method private _make_complex_path =
      Printf.sprintf "M %d %d L %d %d Q %d %d, %d %d Q %d %d, %d %d"

  end
