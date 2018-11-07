open Containers
open Components
open Widget_types
open Board_types
open Chartjs
open Common

type 'a point = (Time.t, 'a) Line.point

type 'a data = (Stream.ID.t * ('a point list)) list

type dataset = (Time.t, float) Line.Dataset.t

type widget_config =
  { sources : data_source list
  ; typ : measure_type
  ; duration : Time.Period.t
  ; settings : widget_settings option
  }
and widget_settings =
  { range : (float * float) option
  }
and data_source = Stream.ID.t [@@deriving yojson, eq]

let base_class = "dvb-niit-measures-line-chart"

let colors =
  Random.init 255;
  let st = Random.get_state () in
  Array.init 100 (fun _ ->
      Random.run ~st (Random.int 255),
      Random.run ~st (Random.int 255),
      Random.run ~st (Random.int 255))

let get_suggested_range = function
  | `Power -> (-70.0, 0.0)
  | `Mer -> (0.0, 45.0)
  | `Ber -> (0.0, 0.00001)
  | `Freq -> (-10.0, 10.0)
  | `Bitrate -> (0.0, 1.0)

let make_settings (settings : widget_settings) =
  let range_min =
    new Textfield.t
      ~label:"Min"
      ~input_type:(Float (None, None))
      () in
  let range_max =
    new Textfield.t
      ~label:"Max"
      ~input_type:(Float (None, None))
      () in
  let box = new Hbox.t ~widgets:[range_min;range_max] () in
  let s =
    React.S.l2 ~eq:(Equal.option equal_widget_settings)
      (fun min max ->
        match min, max with
        | Some min, Some max -> Some { range = Some (min, max) }
        | _ -> None) range_min#s_input range_max#s_input
  in
  box, s

let make_x_axis ?(id = "x-axis") (config : widget_config)
    : (Time.t, Time.span) Line.Axes.Time.t =
  let delta = config.duration in
  let axis =
    new Line.Axes.Time.t
      ~delta
      ~id
      ~position:`Bottom
      ~typ:Ptime
      () in
  axis#scale_label#set_display true;
  axis#scale_label#set_label_string "Время";
  axis#time#set_tooltip_format "ll HH:mm:ss";
  axis#ticks#set_auto_skip_padding 2;
  axis

let make_y_axis ?(id = "y-axis") (config : widget_config) =
  let range = get_suggested_range config.typ in
  let set_range axis = function
    | Some (min, max) ->
       axis#ticks#set_min (Some min);
       axis#ticks#set_max (Some max)
    | None ->
       axis#ticks#set_min None;
       axis#ticks#set_max None in
  let axis, set_range = match config.typ with
    | `Ber ->
       let axis =
         new Chartjs.Line.Axes.Logarithmic.t
           ~id
           ~position:`Left
           ~typ:Float
           () in
       axis#coerce_common, set_range axis
    | _ ->
       let axis =
         new Chartjs.Line.Axes.Linear.t
           ~id:"y-axis"
           ~position:`Left
           ~typ:Float
           () in
       axis#ticks#set_suggested_min (fst range);
       axis#ticks#set_suggested_max (snd range);
       axis#coerce_common, set_range axis in
  axis#scale_label#set_display true;
  axis#scale_label#set_label_string @@ measure_type_to_unit config.typ;
  axis, set_range

let make_options ~x_axes ~y_axes
      (config : widget_config) : Line.Options.t =
  let deferred =
    object%js
      val xOffset = 150
      val yOffset = Js.string "50%"
      val delay = 500
    end in
  let options = new Line.Options.t ~x_axes ~y_axes () in
  (Js.Unsafe.coerce options#get_obj)##.deferred := deferred;
  if List.length config.sources < 2
  then options#legend#set_display false;
  options#set_maintain_aspect_ratio false;
  options#set_responsive true;
  options#animation#set_duration 0;
  options#hover#set_animation_duration 0;
  options#set_responsive_animation_duration 0;
  options

let make_dataset ~x_axis ~y_axis id src data =
  let label = Printf.sprintf "%s" module_name in
  let ds =
    new Line.Dataset.t ~label
      ~data
      ~x_axis
      ~y_axis
      () in
  let (r, g, b) = colors.(id) in
  let color = Color.rgb r g b in
  ds#set_line_tension 0.;
  ds#set_bg_color color;
  ds#set_border_color color;
  ds#set_cubic_interpolation_mode `Monotone;
  ds#set_fill `Disabled;
  src, ds

let make_datasets ~x_axis ~y_axis
      (init : float data)
      (sources : Stream.ID.t list)
    : (Stream.ID.t * dataset) list =
  let map id (src : Stream.ID.t) =
    let data =
      List.find_map (fun (src', data) ->
          if Stream.ID.equal src src'
          then Some data else None) init
      |> Option.get_or ~default:[] in
    make_dataset ~x_axis ~y_axis id src data in
  List.mapi map sources

type init = (Stream.ID.t * Measure.t Time.timestamped list) list
type event = (Stream.t * Measure.t Time.timestamped) React.event

let to_init (get : Measure.t -> float option)
      (init : init) : float data =
  List.map (fun (id, meas) ->
      let data =
        List.map (fun Time.{ data; timestamp } ->
            let y = Option.get_or ~default:nan (get data) in
            ({ x = timestamp; y } : 'a point)) meas in
      id, data) init

let to_event (get : Measure.t -> float option)
      (event : event) : float data React.event =
  React.E.map (fun ((s : Stream.t), Time.{ data; timestamp }) ->
      let y = Option.get_or ~default:nan (get data) in
      [s.id, List.return ({ x = timestamp; y } : 'a point)])
    event

class t ~(config : widget_config)
        ~(init : init)
        ~(event : event)
        ~(getter : Measure.t -> float option)
        () =
  let init = to_init getter init in
  let x_axis = make_x_axis config in
  let y_axis, set_range = make_y_axis config in
  let (event : float data React.event) = to_event getter event in
  let (options : Line.Options.t) =
    make_options ~x_axes:[x_axis] ~y_axes:[y_axis] config in
  let (datasets : (Stream.ID.t * dataset) list) =
    make_datasets ~x_axis ~y_axis init config.sources in
  let chart =
    new Line.t
      ~options
      ~datasets:(List.map snd datasets)
      () in
  object(self)

    val mutable _datasets = datasets

    inherit Widget.t Dom_html.(createDiv document) () as super

    method init () : unit =
      super#init ();
      super#add_class base_class;
      super#append_child chart;
      React.E.map (fun d ->
          List.iter (fun ((s : Stream.ID.t), data) ->
              match List.Assoc.get ~eq:equal_data_source s datasets with
              | None -> ()
              | Some ds ->
                 List.iter ds#push data;
                 chart#update None) d)
        event
      |> self#_keep_e

  end

let get_power (m : Measure.t) = m.power
let get_mer (m : Measure.t) = m.mer
let get_ber (m : Measure.t) = m.ber
let get_freq (m : Measure.t) =
  Option.map float_of_int m.freq
let get_bitrate (m : Measure.t) =
  Option.map (fun b -> float_of_int b /. 1_000_000.) m.bitrate

module type M = sig
  type t
  val to_float : t -> float
end

module Make(M : M) = struct

  let make ~(init : init)
        ~(event : event)
        (config : widget_config) =
    let getter = match config.typ with
      | `Power -> get_power
      | `Mer -> get_mer
      | `Ber -> get_ber
      | `Freq -> get_freq
      | `Bitrate -> get_bitrate in
    new t ~init ~getter ~event ~config ()

end

module Float = struct
  type t = float
  let to_float t = t
end
module Int = struct
  type t = int
  let to_float = float_of_int
end

module Power = Make(Float)
module Mer = Make(Float)
module Ber = Make(Float)
module Freq = Make(Int)
module Bitrate = Make(Float)

let make
      ~(init : (Stream.ID.t * Measure.t Time.timestamped list) list)
      ~(measures : (Stream.t * Measure.t Time.timestamped) React.event)
      (config : widget_config) =
  let event = match config.sources with
    | [] -> measures
    | ids ->
       React.E.filter (fun ((s : Stream.t), _) ->
           List.mem ~eq:Stream.ID.equal s.id ids) measures in
  (match config.typ with
   | `Power -> Power.make ~init ~event config
   | `Mer -> Mer.make ~init ~event config
   | `Ber -> Ber.make ~init ~event config
   | `Freq -> Freq.make ~init ~event config
   | `Bitrate -> Freq.make ~init ~event config)
