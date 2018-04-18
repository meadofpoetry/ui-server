open Containers
open Components
open Widget_types
open Board_types

type 'a point  = (float,'a) Chartjs.Line.point
type 'a data   = (int * ('a point list)) list
type x_axis    = int64 Chartjs.Axes.Cartesian.Time.t
type 'a y_axis = 'a Chartjs.Axes.Cartesian.Linear.t
type 'a chart  = (int64,x_axis,'a,'a y_axis) Chartjs.Line.t

type config =
  { ids      : int list
  ; typ      : measure_type
  ; duration : int64
  }

let colors = Color.([ Indigo C500; Amber C500; Green C500; Cyan C500 ])

let make_chart_base (* ~(typ:    'a Chartjs.Axes.numeric) *)
                    ~(config: config)
                    ~(init:   float data)
                    ~(event:  float data React.event)
                    () : Widget.widget =
  let conv = fun p -> Chartjs.Line.({ p with x = Int64.of_float (p.x *. 1000.) }) in
  let init = List.map (fun x -> match List.Assoc.get ~eq:Int.equal x init with
                                | Some i -> x,List.map conv i
                                | None   -> x,[]) config.ids in
  let data = List.map (fun (id,data) -> Chartjs.Line.({ data; label = Printf.sprintf "Модуль %d" @@ succ id }))
                      init
  in
  let conf = Chartjs.Line.(new Config.t
                               ~x_axis:(Time ("my-x-axis",Bottom,Unix,Some config.duration))
                               ~y_axis:(Linear ("my-y-axis",Left,Float,None))
                               ~data
                               ())
  in
  List.iteri (fun i x -> let clr = Option.get_or ~default:(Color.Red C500) @@ List.get_at_idx i colors in
                         x#set_background_color @@ Color.rgb_of_name clr;
                         x#set_border_color     @@ Color.rgb_of_name clr;
                         x#set_cubic_interpolation_mode Chartjs.Line.Monotone;
                         x#set_fill Chartjs.Line.Disabled)
             conf#datasets;
  conf#options#x_axis#ticks#set_auto_skip_padding 2;
  conf#options#x_axis#scale_label#set_display true;
  conf#options#x_axis#scale_label#set_label_string "Время";
  conf#options#y_axis#scale_label#set_display true;
  conf#options#y_axis#scale_label#set_label_string @@ measure_type_to_unit config.typ;
  conf#options#set_maintain_aspect_ratio false;
  (* set_range conf#options#y_axis config.typ; *)
  let chart = new Chartjs.Line.t ~config:conf () in
  let set   = fun ds data -> List.iter (fun point -> ds#push point) @@ List.map conv data;
                             chart#update None
  in
  let _ = React.E.map (fun datasets -> List.iter (fun (id,data) -> Option.iter (fun ds -> set ds data)
                                                                   @@ List.get_at_idx id chart#config#datasets)
                                                 datasets)
                      event in
  let title = new Card.Primary.title (measure_type_to_string config.typ) () in
  let prim  = new Card.Primary.t ~widgets:[title#widget] () in
  let media = new Card.Media.t ~widgets:[chart#widget] () in
  let card  = new Card.t ~widgets:[prim#widget;media#widget] () in
  card#widget

type event = measure_response React.event
let to_event (get: Board_types.measure -> 'a option)
             (event: event) : 'a data React.event =
  React.E.map (fun (id,meas) -> [id, List.return ({ x = meas.timestamp; y = Option.get_exn (get meas) }:'a point)])
  @@ React.E.filter (fun (_,m) -> Option.is_some @@ get m) event

let to_power_event   (event:event) = to_event (fun m -> m.power)   event
let to_mer_event     (event:event) = to_event (fun m -> m.mer)     event
let to_ber_event     (event:event) = to_event (fun m -> m.ber)     event
let to_freq_event    (event:event) = to_event (fun m -> Option.map Int32.to_float m.freq)    event
let to_bitrate_event (event:event) = to_event (fun m -> Option.map Int32.to_float m.bitrate) event

module type M = sig
  type t
  val to_float  : t -> float
end

module Make(M:M) = struct

  let make ~(init:M.t data) ~(event:event) (config:config) =
    let conv = List.map (fun (id,data) ->
                   id,List.map (fun (x:M.t point) -> ({ x with y = M.to_float x.y }:float point)) data)
    in
    let init  = conv init in
    match config.typ with
    | `Power   -> let event = to_power_event event in make_chart_base ~init ~event ~config ()
    | `Mer     -> let event = to_mer_event event in make_chart_base ~init ~event ~config ()
    | `Ber     -> let event = to_ber_event event in make_chart_base ~init ~event ~config ()
    | `Freq    -> let event = to_freq_event event in make_chart_base ~init ~event ~config ()
    | `Bitrate -> let event = to_bitrate_event event in make_chart_base ~init ~event ~config ()

end

module Float = struct
  type t = float
  let to_float t = t
end
module Int32 = struct
  type t = int32
  let to_float = Int32.to_float
end

module Power   = Make(Float)
module Mer     = Make(Float)
module Ber     = Make(Float)
module Freq    = Make(Int32)
module Bitrate = Make(Int32)
