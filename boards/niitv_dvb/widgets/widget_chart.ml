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
  let legend_wrapper = BEM.add_element root "legend-wrapper"
  let legend_module = BEM.add_element root "legend-module"
  let legend_dataset = BEM.add_element root "legend-dataset"
  let legend_color = BEM.add_element root "legend-color"
end

type data = (int * Measure.t ts list) list

type event =
  [ `Data of data
  | `Mode of (int * Device.mode) list
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

let format_module id = Printf.sprintf "Модуль %d" @@ succ id

let format_config { Device. standard; channel = { bw; freq; plp } } =
  let std = Device.standard_to_string ~full:true standard in
  let frq = freq_to_string freq in
  let bw = bw_to_string bw in
  let plp = match standard with
    | T2 -> Printf.sprintf ", PLP %d" plp
    | _ -> "" in
  Printf.sprintf "%s, %s, %s%s" std frq bw plp

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

module Modules = Map.Make(Int)

let make_module_elt ?(classes = []) ?(attrs = []) (id : int) =
  let open Tyxml.Html in
  let classes = CSS.legend_module :: classes in
  td [span ~a:([a_class classes] @ attrs)
        [txt @@ format_module id]]

let make_dataset_elt ?(classes = []) ?(attrs = [])
    (dataset : _ Chartjs.lineDataset Js.t) =
  let open Tyxml.Html in
  let background =
    Js.Optdef.case dataset##.backgroundColor
      (fun () -> "")
      (fun x ->
         Printf.sprintf "background-color: %s;"
         @@ Js.to_string
         @@ Js.Unsafe.coerce x) in
  let border =
    Js.Optdef.case dataset##.borderColor
      (fun () -> "")
      (fun x ->
         Printf.sprintf "border-color: %s;"
         @@ Js.to_string
         @@ Js.Unsafe.coerce x) in
  let color_style = background ^ border in
  let classes = CSS.legend_dataset :: classes in
  div ~a:([a_class classes] @ attrs)
    [ span ~a:[ a_class [CSS.legend_color]
              ; a_style color_style ] []
    ; txt (Js.to_string dataset##.label)
    ]

let make_modules_row id datasets =
  let open Tyxml.Html in
  tr [ make_module_elt id
     ; td (List.map make_dataset_elt datasets)
     ]

let make_legend_items ?(classes = []) ?(attrs = []) rows =
  let open Tyxml.Html in
  let classes = CSS.legend :: classes in
  table ~a:([a_class classes] @ attrs) rows

let legend_callback = fun (chart : Chartjs.chart Js.t) ->
  let (datasets : _ Chartjs.lineDataset Js.t Js.js_array Js.t) =
    chart##.data##.datasets in
  let modules =
    Modules.bindings
    @@ List.fold_left (fun acc dataset ->
        Modules.update (Js.Unsafe.coerce dataset)##.id
          (function
            | None -> Some [dataset]
            | Some datasets -> Some (dataset :: datasets))
          acc) Modules.empty
    @@ Array.to_list
    @@ Js.to_array datasets in
  let items = List.map (fun (id, datasets) ->
      make_modules_row id datasets)
      modules in
  let s = Format.asprintf "%a"
      (Tyxml.Html.pp_elt ())
      (make_legend_items items) in
  Js.string s

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
                      (format_module (Js.Unsafe.coerce dataset)##.id)
                      (format_value v##.y config)))
         in
         Indexable.of_single @@ Js.string text);
  let tooltips = create_tooltip () in
  tooltips##.callbacks := callbacks;
  tooltips##.mode := Interaction_mode.nearest;
  (Js.Unsafe.coerce tooltips)##.axis := Js.string "x";
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
  options##.legendCallback := Js.wrap_callback legend_callback;
  options

let make_dataset src data mode =
  let (r, g, b) = colors.(src) in
  let color = Color.to_hexstring @@ Color.of_rgb r g b in
  Chartjs.(
    let dataset = create_line_dataset () in
    (Js.Unsafe.coerce dataset)##.id := src;
    dataset##.data := Js.array @@ Array.of_list data;
    dataset##.label := Js.string (format_config mode);
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

let make_datasets typ
    (init : data)
    (mode : (int * Device.mode) list)
    (sources : int list) =
  let sources = match sources with
    | [] -> mode
    | sources -> List.filter (fun (id, _) -> List.mem id sources) mode in
  let map ((src : int), mode) =
    let data = match List.find_opt ((=) src % fst) init with
      | None -> []
      | Some (_, data) -> convert_data typ data in
    make_dataset src data mode in
  List.map map sources

class t ~init
    ~mode
    (config : widget_config)
    (elt : Dom_html.element Js.t) = object(self)

  val legend : Dom_html.element Js.t option =
    Element.query_selector elt (Printf.sprintf ".%s" CSS.legend_wrapper)

  val canvas : Dom_html.canvasElement Js.t =
    Js.Unsafe.coerce @@ Element.query_selector_exn elt "canvas"

  val mutable chart = None

  inherit Widget.t elt () as super

  method! init () : unit =
    let x_axis = make_x_axis config in
    let y_axis = make_y_axis config in
    let options = make_options ~x_axes:[x_axis] ~y_axes:[y_axis] config in
    let data = Chartjs.create_data () in
    let datasets = make_datasets config.typ init mode config.sources in
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

  method chart : Chartjs.lineChart Js.t = Option.get chart

  method notify : event -> unit = function
    | `State _state ->
      (* TODO the idea is to insert `null` value after device state changed. *)
      ()
    | `Mode mode ->
      let mode = List.sort (fun a b -> compare (fst a) (fst b)) mode in
      let datasets = Array.to_list @@ Js.to_array self#chart##.data##.datasets in
      let datasets =
        List.sort (fun a b -> compare
                      (Js.Unsafe.coerce a)##.id
                      (Js.Unsafe.coerce b)##.id)
        @@ List.map (fun ((s : int), mode) ->
            match List.find_opt (fun ds -> ds##.id = s) datasets with
            | None -> make_dataset s [] mode
            | Some ds ->
              ds##.label := Js.string (format_config mode);
              Js.Unsafe.coerce ds)
          mode in
      self#chart##.data##.datasets := Js.array @@ Array.of_list datasets;
      self#generate_legend ();
      let config = Chartjs_streaming.create_update_config () in
      config##.preservation := Js._true;
      self#chart##update_withConfig config
    | `Data data ->
      let data = List.map (fun (src, x) -> src, convert_data config.typ x) data in
      let datasets = Array.to_list @@ Js.to_array self#chart##.data##.datasets in
      List.iter (fun ((s : int), data) ->
          match List.find_opt (fun ds -> ds##.id = s) datasets with
          | None -> ()
          | Some ds -> ds##.data := ds##.data##concat (Js.array @@ Array.of_list data))
        data;
      let config = Chartjs_streaming.create_update_config () in
      config##.preservation := Js._true;
      self#chart##update_withConfig config

  method private generate_legend () : unit =
    Option.iter (fun legend ->
        legend##.innerHTML := self#chart##generateLegend)
      legend
end

let make
    ~(init : (int * Measure.t ts list) list)
    ~(mode : (int * Device.mode) list)
    (config : widget_config) : t =
  let elt = Js_of_ocaml_tyxml.Tyxml_js.Html.(
      Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      @@ div ~a:[a_class [CSS.root]]
        [ div ~a:[a_class [CSS.legend_wrapper]] []
        ; div ~a:[a_class [CSS.chart_wrapper]] [canvas []]
        ]) in
  new t ~init ~mode config elt
