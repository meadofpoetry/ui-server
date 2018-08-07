open Containers
open Tyxml_js

module Markup = Components_markup.Split.Make(Xml)(Svg)(Html)

class virtual t ~vertical panel1 panel2 () =
  let panel1_wrapped = Markup.create_panel [ Widget.to_markup panel1 ] () in
  let panel2_wrapped = Markup.create_panel [ Widget.to_markup panel2 ] () in
  let elt      = Markup.create ~vertical panel1_wrapped panel2_wrapped ()
                 |> Tyxml_js.To_dom.of_element in
  let splitter = elt##querySelector (Js.string ("." ^ Markup.splitter_class))
                 |> Js.Opt.to_option |> Option.get_exn |> Js.Unsafe.coerce in
  let s_perc,s_perc_push = React.S.create 50. in
  let panel1 = Widget.create @@ Tyxml_js.To_dom.of_element panel1_wrapped in
  let panel2 = Widget.create @@ Tyxml_js.To_dom.of_element panel2_wrapped in

  object(self)
    inherit Widget.t elt ()

    val mutable _mouseup   = None
    val mutable _mousemove = None
    val mutable _mouseout  = None

    method panel_1 = panel1
    method panel_2 = panel2

    method private _stop_listen = function
      | None   -> ()
      | Some l -> Dom_events.stop_listen l

    method private _stop_drag () =
      self#_stop_listen _mouseup;   _mouseup   <- None;
      self#_stop_listen _mousemove; _mousemove <- None;
      self#_stop_listen _mouseout;  _mouseout  <- None;

    method virtual private _calc_percent : Dom_html.mouseEvent Js.t -> float

    initializer
      Dom_events.listen splitter Dom_events.Typ.mousedown (fun _ e ->
          let open Dom_events in
          match e##.button with
          | 0 ->
             (listen Dom_html.document##.body Typ.mouseup (fun _ _ ->
                  (* NOTE add callback? *)
                  self#_stop_drag ();
                  true) |> fun l -> _mouseup <- Some l);
             (listen self#root Typ.mousemove (fun _ e ->
                  s_perc_push (self#_calc_percent e);
                  true) |> fun l -> _mousemove <- Some l);
             (listen self#root Typ.mouseout (fun _ e ->
                  (match Option.flatten @@ Js.Optdef.to_option
                         @@ Js.Optdef.map e##.relatedTarget Js.Opt.to_option with
                   | None   -> ()
                   | Some e ->
                      if Equal.physical Dom_html.document##.documentElement e
                      then self#_stop_drag ());
                  true) |> fun l -> _mouseout <- Some l);
             false
          | _ -> true) |> ignore;
      React.S.map (fun p ->
          (Js.Unsafe.coerce panel1#style)##.flexGrow := p;
          (Js.Unsafe.coerce panel2#style)##.flexGrow := 100. -. p) s_perc
      |> self#_keep_s

  end
