open Js_of_ocaml
open Components
open Application_types
open Board_niitv_dvb4ch_types

let ( % ) f g x = f (g x)

module Attr = struct
  let datasets = "data-datasets"

  let order = "data-order"
end

module Const = struct
  let delay = 2000

  let ttl = 3000
end

module CSS = struct
  let root = "niitv-dvb4ch-measurements-chart"

  let chart_wrapper = BEM.add_element root "chart-wrapper"

  let title = BEM.add_element root "title"

  let legend = BEM.add_element root "legend"

  let legend_wrapper = BEM.add_element root "legend-wrapper"

  let legend_module = BEM.add_element root "legend-module"

  let legend_dataset = BEM.add_element root "legend-dataset"

  let legend_color = BEM.add_element root "legend-color"

  let legend_hidden = BEM.add_modifier legend "hidden"

  let legend_history = BEM.add_modifier legend "history"
end

type data = (int * Measure.t ts list) list

type event =
  [ `Data of data
  | `Mode of (int * Device.mode) list
  | `State of Topology.state ]

type widget_config =
  { sources : int list
  ; typ : Util.measure_type
  ; settings : widget_settings }

and widget_settings =
  { range : (float * float) option
  ; period : period }

and period =
  [ `Realtime of Time.Period.t
  | `Archive of Time.Range.t ]
[@@deriving yojson, eq]

let make_config ?(sources = []) ?range ?(period = `Realtime (Time.Span.of_int_s 60)) typ
    =
  {sources; typ; settings = {range; period}}

let reds =
  [| "#b10c1d"
   ; "#c21417"
   ; "#cf1719"
   ; "#d8392c"
   ; "#e35745"
   ; "#f57667"
   ; "#f89a90"
   ; "#eac0bd" |]

let blues = [|"#2171b5"; "#4292c6"; "#6baed6"; "#9ecae1"; "#c6dbef"|]

let greens = [|"#1a7232"; "#27823b"; "#339444"; "#69a761"; "#94bb83"; "#bccfb4"|]

let purples = [|"#6a51a3"; "#807dba"; "#9e9ac8"; "#bcbddc"; "#dadaeb"|]

let colors = [|reds; blues; greens; purples|]

let id_of_dataset (ds : _ Chartjs.lineDataset Js.t) : int = (Js.Unsafe.coerce ds)##.id

let set_dataset_id (ds : _ Chartjs.lineDataset Js.t) (id : int) : unit =
  (Js.Unsafe.coerce ds)##.id := id

let order_of_dataset (ds : _ Chartjs.lineDataset Js.t) : int =
  (Js.Unsafe.coerce ds)##.order

let set_dataset_order (ds : _ Chartjs.lineDataset Js.t) (x : int) : unit =
  let id = id_of_dataset ds in
  let colors = colors.(id) in
  let length = Array.length colors in
  let color = if x >= length then colors.(length - 1) else colors.(x) in
  ds##.backgroundColor := Chartjs.Color.of_string color;
  ds##.borderColor := Chartjs.Color.of_string color;
  (Js.Unsafe.coerce ds)##.order := x

let get_suggested_range = function
  | `Power -> -70.0, 0.0
  | `Mer -> 0.0, 45.0
  | `Ber -> 0.0, 0.00001
  | `Freq -> -10.0, 10.0
  | `Bitrate -> 0.0, 1.0

let format_value (v : float) (config : widget_config) : string =
  let unit = Util.measure_type_to_unit config.typ in
  let v =
    match config.typ with
    | `Mer -> Printf.sprintf "%g" v
    | `Ber -> Printf.sprintf "%0.3e" v
    | `Freq -> Printf.sprintf "%g" v
    | `Power -> Printf.sprintf "%g" v
    | `Bitrate -> Printf.sprintf "%g" v
  in
  Printf.sprintf "%s %s" v unit

let format_module id = Printf.sprintf "Модуль %d" @@ succ id

let format_config {Device.standard; channel = {bw; freq; plp}} =
  let std = Device.standard_to_string ~full:true standard in
  let frq = Util.freq_to_string freq in
  let bw = Util.bw_to_string bw in
  let plp =
    match standard with
    | T2 -> Printf.sprintf ", PLP %d" plp
    | _ -> ""
  in
  Printf.sprintf "%s, %s, %s%s" std frq bw plp

let duration_of_period period =
  int_of_float @@ Float.mul 1000. @@ Ptime.Span.to_float_s period

let ttl_of_delay ~duration delay = Const.ttl + delay + duration

let make_y_axis ?(id = "y-axis") (config : widget_config) =
  let open Chartjs in
  let range = get_suggested_range config.typ in
  let axis =
    match config.typ with
    | `Ber ->
        let axis = empty_logarithmic_axis () in
        axis##.afterBuildTicks :=
          Js.wrap_callback (fun _ ticks ->
              ticks##map
                (Js.wrap_callback (fun v _ _ ->
                     let number = Js.number_of_float v in
                     Js.parseFloat @@ number##toFixed 6)));
        coerce_cartesian_axis axis
    | _ ->
        let ticks = empty_linear_ticks () in
        let axis = empty_linear_axis () in
        ticks##.suggestedMin := Js.def @@ fst range;
        ticks##.suggestedMax := Js.def @@ snd range;
        axis##.ticks := ticks;
        coerce_cartesian_axis axis
  in
  let scale_label = empty_scale_label () in
  scale_label##.display := Js._true;
  scale_label##.labelString := Js.string @@ Util.measure_type_to_unit config.typ;
  axis##.id := Js.string id;
  axis##.scaleLabel := scale_label;
  axis##.position := Position.left;
  axis

let make_x_axis ?(id = "x-axis") (config : widget_config) =
  let open Chartjs in
  let format = Js.string "HH:mm:ss" in
  let scale_label = empty_scale_label () in
  scale_label##.display := Js._true;
  scale_label##.labelString := Js.string "Время";
  let display_formats = empty_time_display_formats () in
  display_formats##.second := format;
  display_formats##.minute := format;
  display_formats##.hour := format;
  let time = empty_time_options () in
  time##.isoWeekday := Js._true;
  time##.tooltipFormat := Js.string "ll HH:mm:ss";
  time##.displayFormats := display_formats;
  let ticks = empty_time_ticks () in
  ticks##.autoSkipPadding := 2;
  let axis_type =
    match config.settings.period with
    | `Realtime _ -> Js.string "realtime"
    | `Archive _ -> Js.string "time"
  in
  let axis = empty_time_axis () in
  axis##.id := Js.string id;
  axis##.time := time;
  axis##.ticks := ticks;
  axis##.scaleLabel := scale_label;
  axis##.position := Position.bottom;
  axis##._type := axis_type;
  axis

module Modules = Map.Make (Int)

let datasets_of_event (e : #Dom_html.event Js.t) =
  let target = Dom.eventTarget e in
  let selector = Printf.sprintf "[data-datasets]" in
  Js.Opt.case
    (Element.closest target selector)
    (fun () -> None)
    (fun elt ->
      match Element.get_attribute elt Attr.datasets with
      | None -> None
      | Some attr -> (
          let hidden = Element.has_class elt CSS.legend_hidden in
          try
            Some
              ( elt
              , hidden
              , Option.map int_of_string (Element.get_attribute elt Attr.order)
              , List.map (Js.parseInt % Js.string) @@ String.split_on_char ',' attr )
          with _ -> None))

let make_module_elt
    ?(classes = [])
    ?(attrs = [])
    (id : int)
    (datasets : _ Chartjs.lineDataset Js.t list) =
  let open Tyxml.Html in
  let datasets =
    String.concat ", " @@ List.map (string_of_int % id_of_dataset) datasets
  in
  let classes = CSS.legend_module :: classes in
  td
    [ span
        ~a:([a_class classes; a_user_data "datasets" datasets] @ attrs)
        [txt @@ format_module id] ]

let make_dataset_elt
    ?(classes = [])
    ?(attrs = [])
    (_chart : Chartjs.chart Js.t)
    (_index : int)
    (dataset : _ Chartjs.lineDataset Js.t) =
  let open Tyxml.Html in
  let background =
    Js.Optdef.case
      dataset##.backgroundColor
      (fun () -> "")
      (fun x ->
        Printf.sprintf "background-color: %s;" @@ Js.to_string @@ Js.Unsafe.coerce x)
  in
  let border =
    Js.Optdef.case
      dataset##.borderColor
      (fun () -> "")
      (fun x -> Printf.sprintf "border-color: %s;" @@ Js.to_string @@ Js.Unsafe.coerce x)
  in
  let color_style = background ^ border in
  let order = order_of_dataset dataset in
  let hidden = Js.Optdef.case dataset##.hidden (fun () -> false) Js.to_bool in
  let cons_if b x l = if b then List.cons x l else l in
  let classes =
    CSS.legend_dataset :: classes
    |> cons_if (order > 0) CSS.legend_history
    |> cons_if hidden CSS.legend_hidden
  in
  div
    ~a:
      ([ a_class classes
       ; a_user_data "datasets" @@ string_of_int (id_of_dataset dataset)
       ; a_user_data "order" (string_of_int order) ]
      @ attrs)
    [ span ~a:[a_class [CSS.legend_color]; a_style color_style] []
    ; txt (Js.to_string dataset##.label) ]

let make_modules_row chart id datasets datasets_js =
  let open Tyxml.Html in
  tr
    [ make_module_elt id datasets
    ; td
        (List.map
           (fun dataset ->
             let index = (Js.Unsafe.coerce datasets_js)##indexOf dataset in
             make_dataset_elt chart index dataset)
           datasets) ]

let make_legend_items ?(classes = []) ?(attrs = []) rows =
  let open Tyxml.Html in
  let classes = CSS.legend :: classes in
  table ~a:([a_class classes] @ attrs) rows

let legend_callback (chart : Chartjs.chart Js.t) =
  let (datasets_js : _ Chartjs.lineDataset Js.t Js.js_array Js.t) =
    chart##.data##.datasets
  in
  let modules =
    Modules.bindings
    @@ List.fold_left
         (fun acc dataset ->
           Modules.update
             (id_of_dataset dataset)
             (function
               | None -> Some [dataset]
               | Some datasets -> Some (dataset :: datasets))
             acc)
         Modules.empty
    @@ Array.to_list
    @@ Js.to_array datasets_js
  in
  let items =
    List.map
      (fun (id, datasets) ->
        let datasets =
          List.sort
            (fun a b -> Int.neg @@ compare (order_of_dataset a) (order_of_dataset b))
            datasets
        in
        make_modules_row chart id datasets datasets_js)
      modules
  in
  let s = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) (make_legend_items items) in
  Js.string s

let make_streaming generate_legend period =
  let open Chartjs in
  let duration = duration_of_period period in
  let streaming = Chartjs_streaming.empty_streaming_config () in
  let delay = Const.delay in
  (* streaming##.frameRate := 1.; *)
  streaming##.ttl := Js.def (ttl_of_delay ~duration delay);
  streaming##.delay := delay;
  streaming##.duration := duration;
  streaming##.onRefresh :=
    Js.some
    @@ Js.wrap_callback (fun (chart : chart Js.t) ->
           let datasets = chart##.data##.datasets in
           let datasets' =
             datasets##filter
               Js.(
                 wrap_callback (fun dataset _ _ ->
                     let (dataset : _ lineDataset Js.t) = Unsafe.coerce dataset in
                     if order_of_dataset dataset = 0
                     then _true
                     else Js.bool (dataset##.data##.length <> 0)))
           in
           chart##.data##.datasets := datasets';
           if datasets##.length <> datasets'##.length
           then (
             let conf = Chartjs_streaming.empty_update_config () in
             conf##.preservation := Js._true;
             chart##update_withConfig conf;
             generate_legend ()));
  streaming

let tooltip_callback
    config
    _
    (item : Chartjs.tooltipItem Js.t)
    (data : Chartjs.data Js.t) =
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
              (format_module (id_of_dataset dataset))
              (format_value v##.y config)))
  in
  Chartjs.Indexable.of_single @@ Js.string text

let make_options ~x_axes ~y_axes generate_legend (config : widget_config) =
  let open Chartjs in
  let legend = empty_legend () in
  legend##.display := Js._false;
  let callbacks = empty_tooltip_callbacks () in
  callbacks##.label := Js.def @@ Js.wrap_meth_callback (tooltip_callback config);
  let tooltips = empty_tooltip () in
  tooltips##.callbacks := callbacks;
  tooltips##.mode := Interaction_mode.nearest;
  (Js.Unsafe.coerce tooltips)##.axis := Js.string "x";
  tooltips##.intersect := Js._false;
  let scales = empty_line_scales () in
  scales##.xAxes := Js.array @@ Array.of_list x_axes;
  scales##.yAxes := Js.array @@ Array.of_list y_axes;
  let plugins = Js.Unsafe.obj [||] in
  plugins##.datalabels := Js._false;
  (match config.settings.period with
  | `Realtime period -> plugins##.streaming := make_streaming generate_legend period
  | `Archive _ -> plugins##.streaming := Js._false);
  let options = empty_line_options () in
  options##.scales := scales;
  options##.plugins := plugins;
  options##.legend := legend;
  options##.tooltips := tooltips;
  options##.maintainAspectRatio := Js._false;
  options##.responsive := Js._true;
  options##.responsiveAnimationDuration := 0;
  options##.legendCallback := Js.wrap_callback legend_callback;
  options

let make_dataset src data (mode : Device.mode) =
  let open Chartjs in
  let order = 0 in
  let color = colors.(src).(order) in
  let dataset = empty_line_dataset () in
  set_dataset_id dataset src;
  set_dataset_order dataset order;
  dataset##.data := Js.array @@ Array.of_list data;
  dataset##.label := Js.string (format_config mode);
  dataset##.fill := Line_fill._false;
  dataset##.pointRadius := Scriptable_indexable.of_single 2;
  dataset##.lineTension := 0.;
  dataset##.backgroundColor := Color.of_string color;
  dataset##.borderColor := Color.of_string color;
  dataset##.cubicInterpolationMode := Interpolation_mode.monotone;
  dataset

let convert_data (typ : Util.measure_type) (data : Measure.t ts list) =
  List.map (fun ({data; timestamp} : Measure.t ts) ->
      let float_of_option = function
        | Some x -> x
        | None -> nan
      in
      let y =
        match typ with
        | `Power -> float_of_option data.power
        | `Mer -> float_of_option data.mer
        | `Ber -> float_of_option data.ber
        | `Freq -> float_of_option @@ Option.map float_of_int data.freq
        | `Bitrate -> (
            float_of_option
            @@
            match data.bitrate with
            | None -> None
            | Some x -> Some (float_of_int x /. 1_000_000.))
      in
      Chartjs.create_data_point
        ~x:Chartjs.Time.(of_float_s @@ Ptime.to_float_s timestamp)
        ~y)
  @@ List.sort (fun a b -> Ptime.compare a.timestamp b.timestamp) data

let get_timestamp data =
  let rec aux = function
    | [] -> None
    | (_, hd) :: tl -> (
      match hd with
      | [] -> aux tl
      | {timestamp; _} :: _ -> Some timestamp)
  in
  aux data

let make_datasets
    typ
    (init : data)
    (mode : (int * Device.mode) list)
    (sources : int list) =
  let sources =
    match sources with
    | [] -> mode
    | sources -> List.filter (fun (id, _) -> List.mem id sources) mode
  in
  let map ((src : int), mode) =
    let data =
      match List.find_opt (( = ) src % fst) init with
      | None -> []
      | Some (_, data) -> convert_data typ data
    in
    make_dataset src data mode
  in
  List.map map sources

class t ~init ~mode (config : widget_config) (elt : Dom_html.element Js.t) =
  object (self)
    val legend : Dom_html.element Js.t =
      Option.get @@ Element.query_selector elt (Printf.sprintf ".%s" CSS.legend_wrapper)

    val canvas : Dom_html.canvasElement Js.t =
      Js.Unsafe.coerce @@ Element.query_selector_exn elt "canvas"

    val mutable chart = None

    val mutable listeners = []

    val mutable delay = None

    inherit Widget.t elt () as super

    method! init () : unit =
      let x_axis = make_x_axis config in
      let y_axis = make_y_axis config in
      let options =
        make_options ~x_axes:[x_axis] ~y_axes:[y_axis] self#generate_legend config
      in
      let data = Chartjs.empty_data () in
      let datasets = make_datasets config.typ init mode config.sources in
      data##.datasets := Js.array @@ Array.of_list datasets;
      chart <- Some (Chartjs.chart_from_canvas Chartjs.Chart.line data options canvas);
      self#generate_legend ();
      super#init ()

    method! initial_sync_with_dom () : unit =
      listeners <- Js_of_ocaml_lwt.Lwt_js_events.[clicks legend self#handle_legend_click];
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      Option.iter (fun x -> x##destroy) chart;
      super#destroy ()

    method chart : Chartjs.lineChart Js.t = Option.get chart

    method notify : event -> unit =
      function
      | `State _state ->
          (* TODO the idea is to insert `NaN` value after device state changed
             to add break in the line. *)
          ()
      | `Mode mode -> self#handle_new_mode mode
      | `Data data -> self#handle_new_data data

    method clear () : unit =
      let datasets = Array.to_list @@ Js.to_array self#chart##.data##.datasets in
      List.iter (fun ds -> ds##.data := Js.array [||]) datasets

    method private handle_new_mode mode =
      let rec aux mode acc = function
        | [] -> acc
        | ds :: tl ->
            let present = ds##.label == Js.string (format_config mode) in
            (if present
            then set_dataset_order ds 0
            else
              let point = Js.array_get ds##.data (ds##.data##.length - 1) in
              Js.Optdef.iter point (fun (p : _ Chartjs.dataPoint Js.t) ->
                  let time =
                    Js.Opt.get (Chartjs.Time.cast_float_s p##.x) (fun () -> assert false)
                  in
                  let point =
                    Chartjs.create_data_point
                      ~x:(Chartjs.Time.of_float_s (time +. 0.1))
                      ~y:nan
                  in
                  ignore @@ ds##.data##push point);
              set_dataset_order ds @@ succ @@ order_of_dataset ds);
            aux mode (acc || present) tl
      in
      let mode = List.sort (fun a b -> compare (fst a) (fst b)) mode in
      let datasets = Array.to_list @@ Js.to_array self#chart##.data##.datasets in
      let datasets =
        List.sort (fun a b -> compare (id_of_dataset a) (id_of_dataset b))
        @@ List.flatten
        @@ List.map
             (fun (src, mode) ->
               match List.find_all (fun ds -> id_of_dataset ds = src) datasets with
               | [] -> [make_dataset src [] mode]
               | datasets ->
                   let created = aux mode false datasets in
                   if created then datasets else make_dataset src [] mode :: datasets)
             mode
      in
      self#chart##.data##.datasets := Js.array @@ Array.of_list datasets;
      self#generate_legend ();
      self#update_chart ()

    method private handle_new_data data =
      (match delay, config.settings.period with
      | Some _, _ | _, `Archive _ -> ()
      | None, `Realtime period ->
          Js.Optdef.iter
            (Chartjs_streaming.of_chart_options self#chart##.options)
            (fun streaming ->
              match get_timestamp data with
              | None -> ()
              | Some timestamp ->
                  let now = Ptime_clock.now () in
                  let delay_s = Ptime.Span.to_float_s (Ptime.diff now timestamp) in
                  let delay_ms = Const.delay + int_of_float (1000. *. delay_s) in
                  let ttl =
                    ttl_of_delay ~duration:(duration_of_period period) delay_ms
                  in
                  delay <- Some delay_ms;
                  streaming##.ttl := Js.def ttl;
                  streaming##.delay := delay_ms));
      let data = List.map (fun (src, x) -> src, convert_data config.typ x) data in
      let datasets = Array.to_list @@ Js.to_array self#chart##.data##.datasets in
      List.iter
        (fun ((s : int), data) ->
          let dataset =
            List.find_opt
              (fun ds -> id_of_dataset ds = s && order_of_dataset ds = 0)
              datasets
          in
          match dataset with
          | None -> ()
          | Some ds -> ds##.data := ds##.data##concat (Js.array @@ Array.of_list data))
        data;
      self#update_chart ()

    method private handle_legend_click e _ : unit Lwt.t =
      match datasets_of_event e with
      | None -> Lwt.return_unit
      | Some (target, hidden, order, datasets) ->
          ignore @@ Element.toggle_class target CSS.legend_hidden;
          self#chart##.data##.datasets##forEach
            (Js.wrap_callback (fun ds _ _ ->
                 let id = id_of_dataset ds in
                 let found =
                   match order with
                   | None -> List.mem id datasets
                   | Some o -> List.mem id datasets && o = order_of_dataset ds
                 in
                 if found
                 then (
                   ds##.hidden := Js.bool (not hidden);
                   let selector =
                     Printf.sprintf ".%s[%s=\"%d\"]" CSS.legend_dataset Attr.datasets id
                   in
                   let selector =
                     match order with
                     | None -> selector
                     | Some order ->
                         Printf.sprintf "%s[%s=\"%d\"]" Attr.order selector order
                   in
                   List.iter (fun item ->
                       ignore
                       @@ Element.toggle_class ~force:(not hidden) item CSS.legend_hidden)
                   @@ Element.query_selector_all legend selector)));
          self#update_chart ();
          Lwt.return_unit

    method private update_chart () : unit =
      let config = Chartjs_streaming.empty_update_config () in
      config##.preservation := Js._true;
      self#chart##update_withConfig config

    method private generate_legend () : unit =
      legend##.innerHTML := self#chart##generateLegend
  end

let make
    ~(init : (int * Measure.t ts list) list)
    ~(mode : (int * Device.mode) list)
    (config : widget_config) : t =
  let elt =
    Js_of_ocaml_tyxml.Tyxml_js.Html.(
      Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      @@ div
           ~a:[a_class [CSS.root]]
           [ div ~a:[a_class [CSS.title]] [txt (Util.measure_type_to_string config.typ)]
           ; div ~a:[a_class [CSS.legend_wrapper]] []
           ; div ~a:[a_class [CSS.chart_wrapper]] [canvas []] ])
  in
  new t ~init ~mode config elt
