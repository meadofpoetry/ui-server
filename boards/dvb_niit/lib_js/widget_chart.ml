open Containers
open Components
open Widget_types
open Board_types

type 'a point  = (Common.Time.t,'a) Chartjs.Line.point
type 'a data   = (int * ('a point list)) list
type 'a chart  = (Common.Time.t,'a) Chartjs.Line.t

type settings =
  { range : (float * float) option
  } [@@deriving yojson]

type config =
  { ids      : int list
  ; typ      : measure_type
  ; duration : Common.Time.t
  ; settings : settings option
  } [@@deriving yojson]

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
  let box       = new Box.t ~vertical:false ~widgets:[range_min;range_max] () in
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
  let open Chartjs.Axes in
  let range = get_suggested_range config.typ in
  let init = List.map (fun x -> match List.Assoc.get ~eq:Int.equal x init with
                                | Some i -> x,i
                                | None   -> x,[]) config.ids in
  let data = List.map (fun (id,data) -> Chartjs.Line.({ data; label = Printf.sprintf "Модуль %d" @@ succ id }))
                      init
  in
  let delta  = config.duration in
  let x_axis = new Cartesian.Time.t ~delta ~id:"x-axis" ~position:`Bottom ~typ:Ptime () in
  let y_axis = new Cartesian.Linear.t ~id:"y-axis" ~position:`Left ~typ:Float () in
  let conf = Chartjs.Line.(new Config.t ~x_axis ~y_axis ~data ()) in
  List.iteri (fun i x -> let clr = Option.get_or ~default:(Color.Red C500) @@ List.get_at_idx i colors in
                         x#set_background_color @@ Color.rgb_of_name clr;
                         x#set_border_color     @@ Color.rgb_of_name clr;
                         x#set_cubic_interpolation_mode Chartjs.Line.Monotone;
                         x#set_fill Chartjs.Line.Disabled)
             conf#datasets;
  x_axis#ticks#set_auto_skip_padding 2;
  x_axis#scale_label#set_display true;
  x_axis#scale_label#set_label_string "Время";
  x_axis#time#set_tooltip_format "ll HH:mm:ss";
  y_axis#scale_label#set_display true;
  y_axis#scale_label#set_label_string @@ measure_type_to_unit config.typ;
  y_axis#ticks#set_suggested_min (fst range);
  y_axis#ticks#set_suggested_max (snd range);
  conf#options#set_maintain_aspect_ratio false;
  let chart = new Chartjs.Line.t ~config:conf () in
  let set   = fun ds data -> List.iter (fun point -> ds#push point) data;
                             chart#update None
  in
  let _ = React.E.map (fun datasets -> List.iter (fun (id,data) -> Option.iter (fun ds -> set ds data)
                                                                   @@ List.get_at_idx id chart#config#datasets)
                                                 datasets)
                      event in
  let settings,s_settings = make_settings { range = None } in
  Dashboard.Item.to_item
    ~name:(measure_type_to_string config.typ)
    ~settings:{ widget = settings#widget
              ; ready  = React.S.map Option.is_some s_settings
              ; set    = fun () ->
                         match React.S.value s_settings with
                         | Some s -> (match s.range with
                                      | Some (min,max) -> y_axis#ticks#set_min (Some min);
                                                          y_axis#ticks#set_max (Some max)
                                      | None           -> y_axis#ticks#set_min None;
                                                          y_axis#ticks#set_max None);
                                     chart#update None;
                                     Lwt_result.return ()
                         | None   -> Lwt_result.fail "no settings available"
              }
    chart#widget

type event = measure_response React.event
let to_event (get: Board_types.measure -> 'a option)
             (event: event) : 'a data React.event =
  React.E.map (fun (id,meas) -> [id, List.return ({ x = meas.timestamp; y = Option.get_exn (get meas) }:'a point)])
  @@ React.E.filter (fun (_,m) -> Option.is_some @@ get m) event

let to_power_event   (event:event) = to_event (fun m -> m.power)   event
let to_mer_event     (event:event) = to_event (fun m -> m.mer)     event
let to_ber_event     (event:event) = to_event (fun m -> m.ber)     event
let to_freq_event    (event:event) = to_event (fun m -> Option.map Int32.to_float m.freq)    event
let to_bitrate_event (event:event) = to_event (fun m -> Option.map (fun b ->
                                                            Int32.to_float b /. 1_000_000.) m.bitrate) event

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
module Int32 = struct
  type t       = int32
  let to_float = Int32.to_float
end

module Power   = Make(Float)
module Mer     = Make(Float)
module Ber     = Make(Float)
module Freq    = Make(Int32)
module Bitrate = Make(Float)

let make ~(measures:Board_types.measure_response React.event) (config:config option) =
  let config = Option.get_exn config in (* FIXME *)
  let event = measures in
  (match config.typ with
   | `Power   -> Power.make ~init:[] ~event config
   | `Mer     -> Mer.make   ~init:[] ~event config
   | `Ber     -> Ber.make   ~init:[] ~event config
   | `Freq    -> Freq.make  ~init:[] ~event config
   | `Bitrate -> Freq.make  ~init:[] ~event config)
