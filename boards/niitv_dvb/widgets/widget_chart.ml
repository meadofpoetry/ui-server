open Js_of_ocaml
open Components
open Application_types
open Board_niitv_dvb_types
open Widget_types

let ( % ) f g x = f (g x)

module CSS = struct
  let root = "niitv-dvb4ch-measurements-chart"
  let chart_wrapper = BEM.add_element root "chart-wrapper"
  let legend = BEM.add_element root "legend"
end

type data = (int * Measure.t ts list) list

type event =
  [ `Data of data
  | `State of Topology.state
  ]

type widget_config =
  { sources : int list
  ; typ : measure_type
  ; settings : widget_settings
  }
and widget_settings =
  { range : (float * float) option
  ; period : period
  }
and period =
  [ `Realtime of Time.Period.t
  | `Archive of Time.Range.t
  ] [@@deriving yojson, eq]

let make_config ?(sources = []) ?range
    ?(period = `Realtime (Time.Span.of_int_s 60))
    typ =
  { sources
  ; typ
  ; settings = { range; period }
  }

let colors =
  Random.init 255;
  Array.init 100 (fun _ ->
      Random.int 255,
      Random.int 255,
      Random.int 255)

let duration_of_period : period -> Time.Period.t = function
  | `Realtime x -> x
  | `Archive (_, x) -> x

let get_suggested_range = function
  | `Power -> (-70.0, 0.0)
  | `Mer -> (0.0, 45.0)
  | `Ber -> (0.0, 0.00001)
  | `Freq -> (-10.0, 10.0)
  | `Bitrate -> (0.0, 1.0)

let format_value (v : float) (config : widget_config) : string =
  let unit = measure_type_to_unit config.typ in
  let v = match config.typ with
    | `Mer -> Printf.sprintf "%g" v
    | `Ber -> Printf.sprintf "%0.3e" v
    | `Freq -> Printf.sprintf "%g" v
    | `Power -> Printf.sprintf "%g" v
    | `Bitrate -> Printf.sprintf "%g" v in
  Printf.sprintf "%s %s" v unit

let format_config (id : int) (config : Device.mode option) =
  let prefix = Printf.sprintf "Модуль %d" id in
  match config with
  | None -> prefix
  | Some { Device. standard; channel = { bw; freq; plp } } ->
    let std = Device.standard_to_string ~full:true standard in
    let frq = freq_to_string freq in
    let bw = bw_to_string bw in
    let plp = match standard with
      | T2 -> Printf.sprintf ", PLP: %d" plp
      | _ -> "" in
    Printf.sprintf "%s. %s, %s, %s%s" prefix std frq bw plp

let make_y_axis ?(id = "y-axis") (config : widget_config) =
  let open Chartjs in
  let range = get_suggested_range config.typ in
  let axis = match config.typ with
    | `Ber ->
      let axis = create_logarithmic_cartesian_axis () in
      axis##.afterBuildTicks := Js.wrap_callback (fun _ ticks ->
          ticks##map (Js.wrap_callback (fun v _ _ ->
              let number = Js.number_of_float v in
              Js.parseFloat @@ number##toFixed 6)));
      coerce_cartesian_axis axis
    | _ ->
      let ticks = create_linear_cartesian_ticks () in
      let axis = create_linear_cartesian_axis () in
      ticks##.suggestedMin := Js.def @@ fst range;
      ticks##.suggestedMax := Js.def @@ snd range;
      axis##.ticks := ticks;
      coerce_cartesian_axis axis in
  let scale_label = create_scale_label () in
  scale_label##.display := Js._true;
  scale_label##.labelString := Js.string @@ measure_type_to_unit config.typ;
  axis##.id := Js.string id;
  axis##.scaleLabel := scale_label;
  axis##.position := Position.left;
  axis

let make_x_axis ?(id = "x-axis") (config : widget_config) =
  let open Chartjs in
  let format = Js.string "HH:mm:ss" in
  let scale_label = create_scale_label () in
  scale_label##.display := Js._true;
  scale_label##.labelString := Js.string "Время";
  let display_formats = create_time_display_formats () in
  display_formats##.second := format;
  display_formats##.minute := format;
  display_formats##.hour := format;
  let time = create_time_cartesian_options () in
  time##.isoWeekday := Js._true;
  time##.tooltipFormat := Js.string "ll HH:mm:ss";
  time##.displayFormats := display_formats;
  let ticks = create_time_cartesian_ticks () in
  ticks##.autoSkipPadding := 2;
  let axis_type = match config.settings.period with
    | `Realtime _ -> Js.string "realtime"
    | `Archive _ -> Js.string "time" in
  let axis = create_time_cartesian_axis () in
  axis##.id := Js.string id;
  axis##.time := time;
  axis##.ticks := ticks;
  axis##.scaleLabel := scale_label;
  axis##.position := Position.bottom;
  axis##._type := axis_type;
  axis

let make_options ~x_axes ~y_axes (config : widget_config) =
  let open Chartjs in
  let legend = create_legend () in
  legend##.display := Js._false;
  let callbacks = create_tooltip_callbacks () in
  callbacks##.label := Js.def @@ Js.wrap_meth_callback
      (fun _ (item : tooltipItem Js.t) (data : data Js.t) ->
         let ds_index = item##.datasetIndex in
         let dataset = Js.array_get data##.datasets ds_index in
         let text = Js.Optdef.case dataset
             (fun () -> "")
             (fun dataset ->
                let (dataset : _ lineDataset Js.t) = Js.Unsafe.coerce dataset in
                Js.Optdef.case (Js.array_get dataset##.data item##.index)
                  (fun () -> "")
                  (fun v -> Printf.sprintf "%s: %s"
                      (Js.to_string dataset##.label)
                      (format_value v##.y config)))
         in
         Indexable.of_single @@ Js.string text);
  let tooltips = create_tooltip () in
  tooltips##.callbacks := callbacks;
  tooltips##.mode := Interaction_mode.index;
  tooltips##.intersect := Js._false;
  let scales = create_line_scales () in
  scales##.xAxes := Js.array @@ Array.of_list x_axes;
  scales##.yAxes := Js.array @@ Array.of_list y_axes;
  let plugins = Js.Unsafe.obj [||] in
  plugins##.datalabels := Js._false;
  (match config.settings.period with
   | `Realtime period ->
     let duration =
       int_of_float
       @@ Float.mul 1000.
       @@ Ptime.Span.to_float_s period in
     let streaming = Chartjs_streaming.create () in
     streaming##.delay := 3000;
     streaming##.duration := duration;
     plugins##.streaming := streaming
   | `Archive _ ->
     plugins##.streaming := Js._false);
  let options = create_line_options () in
  options##.scales := scales;
  options##.plugins := plugins;
  options##.legend := legend;
  options##.tooltips := tooltips;
  options##.maintainAspectRatio := Js._false;
  options##.responsive := Js._true;
  options##.responsiveAnimationDuration := 0;
  options##.legendCallback := Js.wrap_callback (fun (chart : chart Js.t) ->
      let (datasets : _ lineDataset Js.t Js.js_array Js.t) =
        chart##.data##.datasets in
      let items =
        Array.map (fun (x : _ lineDataset Js.t) ->
            Printf.sprintf "<span>Dataset %s</span><br>"
              (Js.to_string x##.label))
        @@ Js.to_array datasets in
      let s =
        Printf.sprintf "<div>%s</div>"
          (String.concat "" @@ Array.to_list items) in
      Js.string s);
  options

let make_dataset id src data =
  let label = string_of_int src in
  let (r, g, b) = colors.(id) in
  let color = Color.to_hexstring @@ Color.of_rgb r g b in
  Chartjs.(
    let dataset = create_line_dataset () in
    dataset##.data := Js.array @@ Array.of_list data;
    dataset##.label := Js.string label;
    dataset##.fill := Line_fill._false;
    dataset##.pointRadius := Scriptable_indexable.of_single 2;
    dataset##.lineTension := 0.;
    dataset##.backgroundColor := Color.of_string color;
    dataset##.borderColor := Color.of_string color;
    dataset##.cubicInterpolationMode := Interpolation_mode.monotone;
    dataset)

let convert_data (typ : measure_type) (data : Measure.t ts list) =
  List.map (fun ({ data; timestamp } : Measure.t ts) ->
      let float_of_option = function Some x -> x | None -> nan in
      let y = match typ with
        | `Power -> float_of_option data.power
        | `Mer -> float_of_option data.mer
        | `Ber -> float_of_option data.ber
        | `Freq -> float_of_option @@ Option.map float_of_int data.freq
        | `Bitrate ->
          float_of_option
          @@ match data.bitrate with
          | None -> None
          | Some x -> Some (float_of_int x /. 1_000_000.) in
      Chartjs.create_data_point
        ~x:Chartjs.Time.(of_float_s @@ Ptime.to_float_s timestamp)
        ~y)
  @@ List.sort (fun a b -> Ptime.compare a.timestamp b.timestamp) data

let make_datasets typ (init : data) (sources : int list) =
  let map id (src : int) =
    let data = match List.find_opt ((=) src % fst) init with
      | None -> []
      | Some (_, data) -> convert_data typ data in
    make_dataset id src data in
  List.mapi map sources

class t ~init (config : widget_config) (elt : Dom_html.element Js.t) = object(self)

  val legend : Dom_html.element Js.t option =
    Element.query_selector elt (Printf.sprintf ".%s" CSS.legend)

  val canvas : Dom_html.canvasElement Js.t =
    Js.Unsafe.coerce @@ Element.query_selector_exn elt "canvas"

  val mutable chart = None

  inherit Widget.t elt () as super

  method! init () : unit =
    let x_axis = make_x_axis config in
    let y_axis = make_y_axis config in
    let options = make_options ~x_axes:[x_axis] ~y_axes:[y_axis] config in
    let data = Chartjs.create_data () in
    let datasets = make_datasets config.typ init config.sources in
    data##.datasets := Js.array @@ Array.of_list datasets;
    chart <- Some (Chartjs.chart_from_canvas
                     Chartjs.Chart.line
                     data
                     options
                     canvas);
    self#generate_legend ();
    super#init ()

  method! destroy () : unit =
    Option.iter (fun x -> x##destroy) chart;
    super#destroy ()

  method chart : Chartjs.lineChart Js.t =
    match chart with
    | None -> raise Not_found
    | Some x -> x

  method notify : event -> unit = function
    | `State _state ->
      (* TODO the idea is to insert `null` value after device state changed. *)
      ()
    | `Data data ->
      let data = List.map (fun (src, x) -> src, convert_data config.typ x) data in
      let datasets = Array.to_list @@ Js.to_array self#chart##.data##.datasets in
      List.iter (fun ((s : int), data) ->
          match List.find_opt (fun ds -> Js.parseInt ds##.label = s) datasets with
          | None ->
            let id = List.length datasets in
            let dataset = make_dataset id s data in
            let datasets =
              List.sort (fun a b -> compare a##.label b##.label)
                (dataset :: datasets) in
            self#chart##.data##.datasets := Js.array @@ Array.of_list datasets;
            self#generate_legend ()
          | Some ds ->
            let data = ds##.data##concat (Js.array @@ Array.of_list data) in
            ds##.data := data) data;
      let config = Chartjs_streaming.create_update_config () in
      config##.preservation := Js._true;
      self#chart##update_withConfig config

  method private generate_legend () : unit =
    Option.iter (fun legend ->
        legend##.innerHTML := self#chart##generateLegend)
      legend
end

let make (init : (int * Measure.t ts list) list)
    (config : widget_config) : t =
  let elt = Js_of_ocaml_tyxml.Tyxml_js.Html.(
      Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      @@ div ~a:[a_class [CSS.root]]
        [ div ~a:[a_class [CSS.legend]] []
        ; div ~a:[a_class [CSS.chart_wrapper]] [canvas []]
        ]) in
  new t ~init config elt
