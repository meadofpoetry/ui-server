open Containers
open Components
open Widget_types
open Board_types
open Chartjs
open Common

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
  let time =
    Scales.Cartesian.Time.Time.make
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

let make_options ~x_axes ~y_axes
      (config : widget_config) : Chartjs.Options.t =
  let open Chartjs in
  let scales = Scales.make ~x_axes ~y_axes () in
  Options.make
    ~scales
    ~maintain_aspect_ratio:false
    ~responsive:true
    ~responsive_animation_duration:0
    ()

(* let make_dataset ~x_axis ~y_axis id src data =
 *   let label = Printf.sprintf "%s" module_name in
 *   let ds =
 *     new Line.Dataset.t ~label
 *       ~data
 *       ~x_axis
 *       ~y_axis
 *       () in
 *   let (r, g, b) = colors.(id) in
 *   let color = Color.rgb r g b in
 *   ds#set_point_radius (`Val 2);
 *   ds#set_line_tension 0.;
 *   ds#set_bg_color color;
 *   ds#set_border_color color;
 *   (\* ds#set_cubic_interpolation_mode `Monotone; *\)
 *   ds#set_fill `Disabled;
 *   src, ds
 * 
 * let make_datasets ~x_axis ~y_axis
 *       (init : float data)
 *       (sources : Stream.ID.t list)
 *     : (Stream.ID.t * dataset) list =
 *   let map id (src : Stream.ID.t) =
 *     let data =
 *       List.find_map (fun (src', data) ->
 *           if Stream.ID.equal src src'
 *           then Some data else None) init
 *       |> Option.get_or ~default:[] in
 *     make_dataset ~x_axis ~y_axis id src data in
 *   List.mapi map sources
 * 
 * let to_init (get : Measure.t -> float option)
 *       (init : init) : float data =
 *   List.map (fun ((id : id), meas) ->
 *       let data =
 *         List.map (fun Time.{ data; timestamp } ->
 *             let y = Option.get_or ~default:nan (get data) in
 *             ({ x = timestamp; y } : 'a point)) meas in
 *       id.stream, data) init
 * 
 * let to_event (get : Measure.t -> float option)
 *       (event : event) : float data React.event =
 *   React.E.map (
 *       List.map (fun ((id : id), Time.{ data; timestamp }) ->
 *           let y = Option.get_or ~default:nan (get data) in
 *           id.stream, List.return ({ x = timestamp; y } : 'a point)))
 *     event *)

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
  (* let getter = match widget_config.typ with
   *   | `Power -> get_power
   *   | `Mer -> get_mer
   *   | `Ber -> get_ber
   *   | `Freq -> get_freq
   *   | `Bitrate -> get_bitrate in *)
  (* let init = to_init getter init in *)
  (* let x_axis = make_x_axis widget_config in
   * let y_axis, set_range = make_y_axis widget_config in *)
  (* let (event : float data React.event) = to_event getter event in *)
  (* let (options : Line.Options.t) =
   *   make_options ~x_axes:[] ~y_axes:[] widget_config in *)
  let datasets = [] in
    (* make_datasets ~x_axis ~y_axis init widget_config.sources in *)
  let chart = Widget.create_div () in
    (* new Line.t
     *   (\* ~options *\)
     *   ~datasets:(List.map snd datasets)
     *   () in *)
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
                 (* chart#update None *)) d)
        React.E.never
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
