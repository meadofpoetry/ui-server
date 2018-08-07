open Containers
open Components
open Widget_types
open Board_types

type 'a point  = (Common.Time.t,'a) Chartjs.Line.point
type 'a data   = (int * ('a point list)) list

type settings =
  { range : (float * float) option
  } [@@deriving yojson]

type config =
  { ids      : int list
  ; typ      : measure_type
  ; duration : Common.Time.Period.t
  ; settings : settings option
  } [@@deriving yojson]

let base_class = "dvb-niit-measures-line-chart"

let colors = Color.([ Indigo C500; Amber C500; Green C500; Cyan C500 ])

let get_suggested_range = function
  | `Power   -> (-70.0,0.0)
  | `Mer     -> (0.0,45.0)
  | `Ber     -> (0.0,0.00001)
  | `Freq    -> (-10.0,10.0)
  | `Bitrate -> (0.0,1.0)

let make_settings (settings:settings) =
  let range_min = new Textfield.t ~label:"Min" ~input_id:"range_min" ~input_type:(Float (None,None)) () in
  let range_max = new Textfield.t ~label:"Max" ~input_id:"range_max" ~input_type:(Float (None,None)) () in
  let box       = new Hbox.t ~widgets:[range_min;range_max] () in
  let s         = React.S.l2 (fun min max ->
                      match min,max with
                      | Some min, Some max -> Some { range = Some (min,max) }
                      | _                  -> None) range_min#s_input range_max#s_input
  in
  box,s

let make_chart_base ~(config: config)
      ~(init:   float data)
      ~(event:  float data React.event)
      () : Dashboard.Item.item =
  let settings,s_settings = make_settings { range = None } in
  let range = get_suggested_range config.typ in
  let init = List.map (fun x -> match List.Assoc.get ~eq:Int.equal x init with
                                | Some i -> x,i
                                | None   -> x,[]) config.ids in
  let delta    = config.duration in
  let x_axis   = new Chartjs.Line.Axes.Time.t ~delta ~id:"x-axis" ~position:`Bottom ~typ:Ptime () in
  let f axis   = function
    | Some (min,max) -> axis#ticks#set_min (Some min); axis#ticks#set_max (Some max)
    | None           -> axis#ticks#set_min None; axis#ticks#set_max None
  in
  let y_axis,f = match config.typ with
    | `Ber ->
       let axis = new Chartjs.Line.Axes.Logarithmic.t ~id:"y-axis" ~position:`Left ~typ:Float () in
       axis#coerce_common,f axis
    | _    ->
       let axis = new Chartjs.Line.Axes.Linear.t ~id:"y-axis" ~position:`Left ~typ:Float () in
       axis#ticks#set_suggested_min (fst range);
       axis#ticks#set_suggested_max (snd range);
       axis#coerce_common,f axis
  in
  let options  = new Chartjs.Line.Options.t ~x_axes:[x_axis] ~y_axes:[y_axis] () in
  let datasets = List.map (fun (id,data) ->
                     let label = Printf.sprintf "Модуль %d" @@ succ id in
                     new Chartjs.Line.Dataset.t ~label ~data ~x_axis ~y_axis ())
                   init
  in
  List.iteri (fun i x -> let clr = Option.get_or ~default:(Color.Red C500)
                                   @@ List.get_at_idx i colors in
                         x#set_bg_color @@ Color.(RGB (rgb_of_material clr));
                         x#set_border_color @@ Color.(RGB (rgb_of_material clr));
                         x#set_cubic_interpolation_mode `Monotone;
                         x#set_fill `Disabled)
    datasets;
  x_axis#ticks#set_auto_skip_padding 2;
  x_axis#scale_label#set_display true;
  x_axis#scale_label#set_label_string "Время";
  x_axis#time#set_tooltip_format "ll HH:mm:ss";
  y_axis#scale_label#set_display true;
  y_axis#scale_label#set_label_string @@ measure_type_to_unit config.typ;
  options#set_maintain_aspect_ratio false;
  let chart = new Chartjs.Line.t ~options ~datasets () in
  let set   = fun ds data -> List.iter (fun point -> ds#push point) data;
                             chart#update None
  in
  let _ = React.E.map (fun d ->
              List.iter (fun (id,data) -> Option.iter (fun ds -> set ds data)
                                          @@ List.get_at_idx id datasets)
                d)
            event in
  let box = Widget.create_div () in
  box#add_class base_class;
  box#append_child chart;
  Dashboard.Item.to_item
    ~name:(measure_type_to_string config.typ)
    ~settings:{ widget = settings#widget
              ; ready  = React.S.map Option.is_some s_settings
              ; set    = fun () ->
                         match React.S.value s_settings with
                         | Some s -> f s.range; chart#update None;
                                     Lwt_result.return ()
                         | None   -> Lwt_result.fail "no settings available"
    }
    box

type event = (int * measures) React.event
let to_event (get: Board_types.measures -> float option)
      (event: event) : float data React.event =
  React.E.map (fun (id,m) ->
      let y = Option.get_or ~default:nan (get m) in
      [ id, List.return ({ x = m.timestamp; y }:'a point) ]) event

let to_power_event   (event:event) = to_event (fun m -> m.power)   event
let to_mer_event     (event:event) = to_event (fun m -> m.mer)     event
let to_ber_event     (event:event) = to_event (fun m -> m.ber)     event
let to_freq_event    (event:event) = to_event (fun m -> Option.map float_of_int m.freq)    event
let to_bitrate_event (event:event) = to_event (fun m -> Option.map (fun b ->
                                                            float_of_int b /. 1_000_000.) m.bitrate) event

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
  type t         = float
  let to_float t = t
end
module Int = struct
  type t = int
  let to_float = float_of_int
end

module Power   = Make(Float)
module Mer     = Make(Float)
module Ber     = Make(Float)
module Freq    = Make(Int)
module Bitrate = Make(Float)

let make ~(measures:(int * measures) React.event) (config:config option) =
  let config = Option.get_exn config in (* FIXME *)
  let event = measures in
  (match config.typ with
   | `Power   -> Power.make ~init:[] ~event config
   | `Mer     -> Mer.make   ~init:[] ~event config
   | `Ber     -> Ber.make   ~init:[] ~event config
   | `Freq    -> Freq.make  ~init:[] ~event config
   | `Bitrate -> Freq.make  ~init:[] ~event config)
