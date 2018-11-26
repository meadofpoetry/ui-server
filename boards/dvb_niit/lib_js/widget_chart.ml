open Containers
open Components
open Widget_types
open Board_types
open Common

module Point = struct
  open Chartjs_types
  include Chartjs.Line.Dataset.Make_point(Time)(Float)
end
module Dataset = Chartjs.Line.Dataset.Make(Point)

type data = (Stream.ID.t * Point.t list) list

type init = (id * Measure.t Time.timestamped list) list

type event = (id * Measure.t Time.timestamped) list React.event

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
    : Chartjs.Scales.t =
  let open Chartjs in
  let open Chartjs_plugin_streaming in
  let duration =
    int_of_float
    @@ Ptime.Span.to_float_s config.duration *. 1000. in
  let scale_label =
    Scales.Scale_label.make
      ~display:true
      ~label_string:"Время"
      () in
  let display_formats =
    Scales.Cartesian.Time.Time.Display_formats.make
      ~minute:"HH:mm:ss"
      ~second:"HH:mm:ss"
      ~hour:"HH:mm:ss"
      () in
  let time =
    Scales.Cartesian.Time.Time.make
      ~iso_weekday:true
      ~display_formats
      ~tooltip_format:"ll HH:mm:ss"
      () in
  let ticks =
    Scales.Cartesian.Time.Ticks.make
      ~auto_skip_padding:2
      () in
  let axis =
    Scales.Cartesian.Time.make
      ~id
      ~scale_label
      ~ticks
      ~time
      ~position:`Bottom
      ~type_:(`Custom axis_type)
      () in
  let streaming = make ~duration () in
  Per_axis.set_realtime axis streaming;
  axis

let make_y_axis ?(id = "y-axis") (config : widget_config) =
  let open Chartjs in
  let open Chartjs.Scales.Cartesian in
  let range = get_suggested_range config.typ in
  let scale_label =
    Scales.Scale_label.make
      ~display:true
      ~label_string:(measure_type_to_unit config.typ)
      () in
  let position = `Left in
  let axis, set_range = match config.typ with
    | `Ber ->
       let axis = Logarithmic.make ~id ~scale_label ~position () in
       let set_range = function
         | None ->
            let open Logarithmic in
            let ticks = ticks axis in
            Ticks.set_min ticks None;
            Ticks.set_max ticks None
         | Some (min, max) ->
            let open Logarithmic in
            let ticks = ticks axis in
            Ticks.set_max ticks (Some max);
            Ticks.set_min ticks (Some min) in
       axis, set_range
    | _ ->
       let ticks =
         Linear.Ticks.make
           ~suggested_min:(fst range)
           ~suggested_max:(snd range)
           () in
       let axis = Linear.make ~id ~ticks ~scale_label ~position () in
       let set_range = function
         | None ->
            let open Linear in
            let ticks = ticks axis in
            Ticks.set_min ticks None;
            Ticks.set_max ticks None
         | Some (min, max) ->
            let open Linear in
            let ticks = ticks axis in
            Ticks.set_max ticks (Some max);
            Ticks.set_min ticks (Some min) in
       axis, set_range in
  axis, set_range

let format_value (v : float) (config : widget_config) : string =
  let unit = measure_type_to_unit config.typ in
  let v = match config.typ with
    | `Power -> Printf.sprintf "%g" v
    | `Ber -> Printf.sprintf "%0.3e" v
    | `Mer -> Printf.sprintf "%g" v
    | `Freq -> Printf.sprintf "%g" v
    | `Bitrate -> Printf.sprintf "%g" v in
  Printf.sprintf "%s %s" v unit

let make_options ~x_axes ~y_axes
      (config : widget_config) : Chartjs.Options.t =
  let open Chartjs in
  let scales = Scales.make ~x_axes ~y_axes () in
  let legend = Options.Legend.make ~display:false () in
  let plugins = Options.Plugins.make () in
  let callbacks =
    Options.Tooltips.Callbacks.make
      ~label:(fun item data ->
        let ds_index = item.dataset_index in
        Chartjs_array.(
          let dataset = Any.((Data.datasets data).%[ds_index]) in
          let label = Dataset.label dataset in
          let data = Dataset.data dataset in
          let value = Dataset.A.(data.%[item.index]) in
          Printf.sprintf "%s: %s" label (format_value value.y config)
        ))
      () in
  let tooltips =
    Options.Tooltips.make
      ~callbacks
      ~mode:`Nearest
      ~intersect:false
      () in
  Chartjs_plugin_datalabels.Per_chart.set_datalabels plugins None;
  Options.make
    ~scales
    ~plugins
    ~legend
    ~tooltips
    ~maintain_aspect_ratio:false
    ~responsive:true
    ~responsive_animation_duration:0
    ()

let make_dataset id src (data : Point.t list) =
  let data = List.sort (fun (a : Point.t) b -> Ptime.compare a.x b.x) data in
  let label = Printf.sprintf "%s" module_name in
  let (r, g, b) = colors.(id) in
  let color = Color.to_hexstring @@ Color.of_rgb r g b in
  let ds =
    Dataset.make
      ~data
      ~label
      ~fill:`Off
      ~point_radius:(`Single 2)
      ~line_tension:0.
      ~background_color:color
      ~border_color:color
      ~cubic_interpolation_mode:`Monotone
      () in
  src, ds

let make_datasets (init : data)
      (sources : Stream.ID.t list) =
  let map id (src : Stream.ID.t) =
    let data =
      List.find_map (fun (src', data) ->
          if Stream.ID.equal src src'
          then Some data else None) init
      |> Option.get_or ~default:[] in
    make_dataset id src data in
  List.mapi map sources

let to_init (get : Measure.t -> float option)
      (init : init) : data =
  List.map (fun ((id : id), meas) ->
      let data =
        List.map (fun Time.{ data; timestamp } ->
            let y = Option.get_or ~default:nan (get data) in
            ({ x = timestamp; y } : Point.t)) meas in
      id.stream, data) init

let to_event (get : Measure.t -> float option)
      (event : event) : data React.event =
  React.E.map (
      List.map (fun ((id : id), Time.{ data; timestamp }) ->
          let y = Option.get_or ~default:nan (get data) in
          id.stream, List.return ({ x = timestamp; y } : Point.t)))
    event

let get_power (m : Measure.t) = m.power
let get_mer (m : Measure.t) = m.mer
let get_ber (m : Measure.t) = m.ber
let get_freq (m : Measure.t) =
  Option.map float_of_int m.freq
let get_bitrate (m : Measure.t) =
  Option.map (fun b -> float_of_int b /. 1_000_000.) m.bitrate

class t ~(init : init)
        ~(event : event)
        (widget_config : widget_config)
        () =
  let getter = match widget_config.typ with
    | `Power -> get_power
    | `Mer -> get_mer
    | `Ber -> get_ber
    | `Freq -> get_freq
    | `Bitrate -> get_bitrate in
  let init = to_init getter init in
  let x_axis = make_x_axis widget_config in
  let y_axis, set_range = make_y_axis widget_config in
  let (event : data React.event) = to_event getter event in
  let (options : Chartjs.Options.t) =
    make_options ~x_axes:[x_axis] ~y_axes:[y_axis] widget_config in
  let datasets = make_datasets init widget_config.sources in
  let data = Chartjs.Data.make ~datasets:(List.map snd datasets) () in
  let canvas = Dom_html.(createCanvas document) in
  let line = Chartjs.make ~options ~data `Line (`Canvas canvas) in
  let box = Dom_html.(createDiv document) in
  object(self)

    val mutable _datasets = datasets

    inherit Widget.t box () as super

    method init () : unit =
      super#init ();
      super#append_child @@ Widget.create canvas;
      super#add_class base_class;
      React.E.map (fun d ->
          List.iter (fun ((s : Stream.ID.t), data) ->
              match List.Assoc.get ~eq:equal_data_source s datasets with
              | None -> ()
              | Some ds ->
                 let push v = ignore @@ Dataset.A.push (Dataset.data ds) v in
                 List.iter push data;
                 Chartjs.update line None) d)
        event
      |> self#_keep_e

  end

let make ~(init : init)
      ~(measures : event)
      (widget_config : widget_config) =
  let event = match widget_config.sources with
    | [] -> measures
    | ids ->
       React.E.fmap (fun l ->
           List.filter (fun ((id : id), _) ->
               List.mem ~eq:Stream.ID.equal id.stream ids) l
           |> function
             | [] -> None
             | l -> Some l) measures in
  new t ~init ~event widget_config ()
