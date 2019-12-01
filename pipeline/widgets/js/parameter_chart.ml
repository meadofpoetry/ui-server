open Js_of_ocaml
open Components
open Application_types
open Pipeline_types
include Pipeline_widgets_tyxml.Parameter_chart

module Attr = struct
  let data_typ = "data-type"

  let data_duration = "data-duration"
end

module Const = struct
  include Const

  let delay = 2000

  let ttl = 3000
end

type widget_config =
  { duration : Time.Period.t
  ; typ : Util.measure_typ
  ; sources : data_source list
  ; settings : widget_settings option
  }

and widget_settings = { range : (float * float) option }

and service_filter =
  { service_id : int
  ; pids : int list
  }

and data_source =
  { stream : Stream.ID.t
  ; channel : int
  ; pid : int
  }
[@@deriving eq, yojson]

type event =
  [ `Data of (data_source * Qoe_errors.point array) list
  | `Structures of Structure.Annotated.t
  ]

let sort_data (data : (data_source * Qoe_errors.point array) list) =
  List.map
    (fun (id, x) ->
      Array.sort (fun (a : Qoe_errors.point) b -> Ptime.compare a.time b.time) x;
      id, x)
    data

let colors =
  Random.init 255;
  Array.init 100 (fun _ -> Random.int 255, Random.int 255, Random.int 255)

let get_suggested_range : Util.measure_typ -> float * float = function
  | `Black -> 0.0, 100.0
  | `Luma -> 16.0, 235.0
  | `Freeze -> 0.0, 100.0
  | `Diff -> 0.0, 216.0
  | `Blocky -> 0.0, 100.0
  | `Shortt | `Moment -> -40., 0.

let interpolate : Util.measure_typ -> Qoe_errors.point array -> float =
 fun typ arr ->
  let f =
    match typ with
    | `Black -> max
    | `Luma -> min
    | `Freeze -> max
    | `Diff -> min
    | `Blocky -> max
    | `Shortt | `Moment -> max
  in
  Array.fold_left (fun acc (x : Qoe_errors.point) -> f acc x.data) arr.(0).data arr

let convert_data
    (config : widget_config)
    (data : (data_source * Qoe_errors.point array) list) =
  List.filter_map
    (fun (src, points) ->
      match Array.length points with
      | 0 -> None
      | length ->
          let y = interpolate config.typ points in
          let x =
            Chartjs.Time.of_float_s
            @@ Ptime.to_float_s
            @@ Ptime.truncate ~frac_s:0
            @@ points.(length - 1).time
          in
          Some (src, [| Chartjs.create_data_point ~x ~y |]))
    data

let data_source_to_string (structures : Structure.Annotated.t) (src : data_source) :
    string =
  let open Structure in
  match
    List.find_opt
      (fun (_, (x : Annotated.structure)) -> Stream.ID.equal x.id src.stream)
      structures
  with
  | None -> ""
  | Some (_, { channels; _ }) -> (
      match
        List.find_opt
          (fun (_, (x : Annotated.channel)) -> src.channel = x.number)
          channels
      with
      | None -> ""
      | Some (_, channel) -> (
          match List.find_opt (fun (_, (x : pid)) -> x.pid = src.pid) channel.pids with
          | None -> ""
          | Some (_, pid) ->
              Printf.sprintf
                "%s. PID %d (%s)"
                channel.service_name
                pid.pid
                pid.stream_type_name))

let get_duration_ms config =
  int_of_float @@ (Ptime.Span.to_float_s config.duration *. 1000.)

let ttl_of_delay ~duration delay = Const.ttl + delay + duration

let get_timestamp (data : (data_source * Qoe_errors.point array) list) =
  let rec aux = function
    | [] -> None
    | (_, hd) :: tl ->
        if Array.length hd = 0
        then aux tl
        else
          let (point : Qoe_errors.point) = hd.(0) in
          Some point.time
  in
  aux data

let make_x_axis ?(id = "x-axis") () : Chartjs.timeAxis Js.t =
  let open Chartjs in
  let scale_label = empty_scale_label () in
  scale_label##.display := Js._true;
  scale_label##.labelString := Js.string "Время";
  let time_format = "HH:mm:ss" in
  let display_formats = empty_time_display_formats () in
  display_formats##.second := Js.string time_format;
  display_formats##.minute := Js.string time_format;
  display_formats##.hour := Js.string time_format;
  let time_options = empty_time_options () in
  time_options##.isoWeekday := Js._true;
  time_options##.displayFormats := display_formats;
  time_options##.tooltipFormat := Js.string "ll HH:mm:ss";
  let ticks = empty_time_ticks () in
  ticks##.autoSkipPadding := 2;
  let axis = empty_time_axis () in
  axis##.id := Js.string id;
  axis##.scaleLabel := scale_label;
  axis##.ticks := ticks;
  axis##.time := time_options;
  axis##.position := Position.bottom;
  axis##._type := Js.string "realtime";
  axis

let make_y_axis ?(id = "y-axis") (config : widget_config) : Chartjs.linearAxis Js.t =
  let open Chartjs in
  let min, max = get_suggested_range config.typ in
  let unit = Util.measure_typ_to_unit_string config.typ in
  let scale_label = empty_scale_label () in
  scale_label##.display := Js._true;
  scale_label##.labelString := Js.string unit;
  let ticks = empty_linear_ticks () in
  ticks##.suggestedMin := Js.def min;
  ticks##.suggestedMax := Js.def max;
  let axis = empty_linear_axis () in
  axis##.id := Js.string id;
  axis##.ticks := ticks;
  axis##.scaleLabel := scale_label;
  axis##.position := Position.left;
  axis

let make_streaming config =
  let duration = get_duration_ms config in
  let streaming = Chartjs_streaming.empty_streaming_config () in
  streaming##.delay := Const.delay;
  streaming##.frameRate := 1.;
  (* streaming##.ttl := Js.def (ttl_of_delay ~duration Const.delay); *)
  streaming##.duration := duration;
  streaming

(* let legend_callback (chart : Chartjs.chart Js.t) =
 *   let (datasets_js : _ Chartjs.lineDataset Js.t Js.js_array Js.t) =
 *     chart##.data##.datasets
 *   in
 *   let modules =
 *     Modules.bindings
 *     @@ List.fold_left
 *          (fun acc dataset ->
 *            Modules.update
 *              (id_of_dataset dataset)
 *              (function
 *                | None -> Some [ dataset ]
 *                | Some datasets -> Some (dataset :: datasets))
 *              acc)
 *          Modules.empty
 *     @@ Array.to_list
 *     @@ Js.to_array datasets_js
 *   in
 *   let items =
 *     List.map
 *       (fun (id, datasets) ->
 *         let datasets =
 *           List.sort
 *             (fun a b -> Int.neg @@ compare (order_of_dataset a) (order_of_dataset b))
 *             datasets
 *         in
 *         make_modules_row chart id datasets datasets_js)
 *       modules
 *   in
 *   let s = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) (make_legend_items items) in
 *   Js.string s *)

let strip str = Str.replace_first (Str.regexp "0+$") "" str

let format_float ?(decimals = 3) ?(strip_zero = false) (v : float) =
  match String.split_on_char '.' (Printf.sprintf "%.*f" decimals v) with
  | [ s ] -> s
  | [ left; right ] -> (
      let right = if strip_zero then strip right else right in
      match right with
      | "" -> left
      | _ -> left ^ "." ^ right)
  | _ -> assert false

let format_value (v : float) (config : widget_config) : string =
  let unit = Util.measure_typ_to_unit_string config.typ in
  Printf.sprintf "%s %s" (format_float v) unit

let tooltip_callback config _ (item : Chartjs.tooltipItem Js.t) (data : Chartjs.data Js.t)
    =
  let ds_index = item##.datasetIndex in
  let dataset = Js.array_get data##.datasets ds_index in
  let text =
    Js.Optdef.case
      dataset
      (fun () -> "")
      (fun dataset ->
        let (dataset : _ Chartjs.lineDataset Js.t) = Js.Unsafe.coerce dataset in
        Js.Optdef.case
          (Js.array_get dataset##.data item##.index)
          (fun () -> "")
          (fun v ->
            Printf.sprintf
              "%s: %s"
              (Js.to_string dataset##.label)
              (format_value v##.y config)))
  in
  Chartjs.Indexable.of_single @@ Js.string text

let make_options ~x_axes ~y_axes config =
  let open Chartjs in
  let callbacks = empty_tooltip_callbacks () in
  let tooltips = empty_tooltip () in
  let scales = empty_line_scales () in
  let animation = empty_animation () in
  let hover = empty_hover () in
  let options = empty_line_options () in
  let plugins = Js.Unsafe.obj [||] in
  plugins##.streaming := make_streaming config;
  plugins##.datalabels := Js._false;
  animation##.duration := 0;
  callbacks##.label := Js.def @@ Js.wrap_meth_callback (tooltip_callback config);
  tooltips##.callbacks := callbacks;
  tooltips##.mode := Interaction_mode.index;
  tooltips##.intersect := Js._false;
  scales##.xAxes := Js.array @@ Array.of_list x_axes;
  scales##.yAxes := Js.array @@ Array.of_list y_axes;
  hover##.animationDuration := 0;
  options##.animation := animation;
  options##.scales := scales;
  options##.tooltips := tooltips;
  options##.hover := hover;
  options##.responsiveAnimationDuration := 0;
  options##.maintainAspectRatio := Js._false;
  options##.responsive := Js._true;
  options##.plugins := plugins;
  options

let make_dataset id src structures data =
  let r, g, b = colors.(id) in
  let color = Color.to_hexstring @@ Color.of_rgb r g b in
  (* TODO implement label update on structure update *)
  let label = data_source_to_string structures src in
  let ds = Chartjs.empty_line_dataset () in
  ds##.data := Js.array data;
  ds##.fill := Chartjs.Line_fill._false;
  ds##.label := Js.string label;
  ds##.lineTension := 0.;
  ds##.pointRadius := Chartjs.Scriptable_indexable.of_single 2;
  ds##.backgroundColor := Chartjs.Color.of_string color;
  ds##.borderColor := Chartjs.Color.of_string color;
  src, ds

let make_datasets init (sources : data_source list) (structures : Structure.Annotated.t) =
  let map id (src : data_source) =
    let data =
      List.find_opt (fun (src', _) -> equal_data_source src src') init
      |> function
      | None -> [||]
      | Some (_, x) -> x
    in
    make_dataset id src structures data
  in
  List.mapi map sources

class t
  (init : 'a list)
  (structures : Structure.Annotated.t)
  (config : widget_config)
  (elt : Dom_html.element Js.t) =
  object (self)
    val canvas : Dom_html.canvasElement Js.t =
      Js.Unsafe.coerce @@ Element.query_selector_exn elt "canvas"

    val mutable datasets =
      let sorted = sort_data init in
      let data = convert_data config sorted in
      make_datasets data config.sources structures
    (** Chart datasets. *)

    val mutable chart = None
    (** Chartjs instance. *)

    val mutable delay = None
    (** Estimated delay between device and client time in milliseconds. *)

    inherit Widget.t elt () as super

    method! init () : unit =
      let x_axis = make_x_axis () in
      let y_axis = make_y_axis config in
      let options = make_options ~x_axes:[ x_axis ] ~y_axes:[ y_axis ] config in
      let data = Chartjs.empty_data () in
      data##.datasets := Js.array @@ Array.of_list @@ List.map snd datasets;
      chart <- Some (Chartjs.chart_from_canvas Chartjs.Chart.line data options canvas);
      super#init ()

    method! destroy () : unit =
      Option.iter (fun x -> x##destroy) chart;
      super#destroy ()

    method chart : Chartjs.lineChart Js.t = Option.get chart

    method typ : Util.measure_typ = config.typ

    method notify : event -> unit =
      function
      (* TODO add structures and state update *)
      | `Structures structures -> self#update_structures structures
      | `Data data -> self#handle_new_data data

    method clear () : unit =
      self#chart##.data##.datasets := Js.array [||];
      datasets <- [];
      self#update_chart ()

    method private update_structures (structures : Structure.Annotated.t) : unit =
      List.iter
        (fun (src, (ds : _ Chartjs.lineDataset Js.t)) ->
          let label = data_source_to_string structures src in
          ds##.label := Js.string label)
        datasets

    method private update_delay data =
      match delay with
      | Some _ -> ()
      | None ->
          Js.Optdef.iter
            (Chartjs_streaming.of_chart_options self#chart##.options)
            (fun streaming ->
              match get_timestamp data with
              | None -> ()
              | Some timestamp ->
                  let now = Ptime_clock.now () in
                  let delay_s = Ptime.Span.to_float_s (Ptime.diff now timestamp) in
                  let delay_ms = Const.delay + int_of_float (1000. *. delay_s) in
                  delay <- Some delay_ms;
                  streaming##.delay := delay_ms)

    method private handle_new_data (data : (data_source * Qoe_errors.point array) list) =
      let sorted = sort_data data in
      self#update_delay sorted;
      let converted = convert_data config sorted in
      List.iter
        (fun (src, data) ->
          match List.find_opt (fun (x, _) -> equal_data_source src x) datasets with
          | None -> (
              match config.sources with
              | [] ->
                  let id = List.length datasets in
                  let ds = make_dataset id src structures data in
                  datasets <- ds :: datasets;
                  let (_ : int) =
                    self#chart##.data##.datasets##push (Chartjs.coerce_dataset @@ snd ds)
                  in
                  ()
              | _ -> ())
          | Some (_, (ds : _ Chartjs.lineDataset Js.t)) ->
              Array.iter (fun x -> ignore @@ ds##.data##push x) data;
              let sort (a : _ Chartjs.dataPoint Js.t as 'c) (b : 'c) =
                float_of_int @@ compare a##.x b##.x
              in
              let data = ds##.data##sort (Js.wrap_callback sort) in
              ds##.data := data)
        converted;
      self#update_chart ()

    method private update_chart () : unit =
      let config = Chartjs_streaming.empty_update_config () in
      config##.preservation := Js._true;
      self#chart##update_withConfig config
  end

let make init structures config =
  let elt =
    Js_of_ocaml_tyxml.Tyxml_js.Html.(
      Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      @@ div ~a:[ a_class [ CSS.root ] ] [ canvas [] ])
  in
  new t init structures config elt

let attach ?duration ?typ ~init ~structures (elt : Dom_html.element Js.t) : t =
  let duration =
    match duration with
    | Some x -> x
    | None ->
        Option.value ~default:Const.default_duration
        @@ Option.bind (Element.get_attribute elt Attr.data_duration) duration_of_string
  in
  let typ =
    match typ with
    | Some x -> x
    | None ->
        Option.value ~default:`Black
        @@ Option.bind
             (Element.get_attribute elt Attr.data_typ)
             Util.measure_typ_of_string
  in
  let config = { duration; typ; sources = []; settings = None } in
  new t init structures config elt
