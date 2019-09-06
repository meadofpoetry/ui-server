open Js_of_ocaml
open Components
open Application_types
open Pipeline_types

module CSS = struct
  let root = "pipeline-chart"
end

type widget_config =
  { duration : Time.Period.t
  ; typ : typ
  ; sources : data_source list
  ; settings : widget_settings option }

and widget_settings = {range : (float * float) option}

and service_filter =
  { service_id : int
  ; pids : int list }

and data_source =
  { stream : Stream.ID.t
  ; service : int
  ; pid : int }
[@@deriving eq, yojson]

and typ =
  [ `Black
  | `Luma
  | `Freeze
  | `Diff
  | `Blocky
  | `Shortt
  | `Moment ]

type event = [`Data of (data_source * kind) list]

and kind =
  [ `Video of Qoe_errors.Video_data.data
  | `Audio of Qoe_errors.Audio_data.data ]

let filter_data typ (data : (data_source * kind) list) =
  List.filter_map
    (fun (src, kind) ->
      match typ, (kind : kind) with
      | `Black, `Video x -> Some (src, x.black)
      | `Luma, `Video x -> Some (src, x.luma)
      | `Freeze, `Video x -> Some (src, x.freeze)
      | `Diff, `Video x -> Some (src, x.diff)
      | `Blocky, `Video x -> Some (src, x.blocky)
      | `Shortt, `Audio x -> Some (src, x.shortt)
      | `Moment, `Audio x -> Some (src, x.moment)
      | _ -> None)
    data

let colors =
  Random.init 255;
  Array.init 100 (fun _ -> Random.int 255, Random.int 255, Random.int 255)

let get_suggested_range : typ -> float * float = function
  | `Black -> 0.0, 100.0
  | `Luma -> 16.0, 235.0
  | `Freeze -> 0.0, 100.0
  | `Diff -> 0.0, 216.0
  | `Blocky -> 0.0, 100.0
  | `Shortt | `Moment -> -40., 0.

let interpolate : typ -> Qoe_errors.point array -> float =
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
      Array.sort (fun (a : Qoe_errors.point) b -> Ptime.compare a.time b.time) points;
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
          Some (src, [|Chartjs.create_data_point ~x ~y|]))
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
  | Some (_, {channels; _}) -> (
    match
      List.find_opt (fun (_, (x : Annotated.channel)) -> src.service = x.number) channels
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

let typ_to_content : typ -> [`Video | `Audio] = function
  | `Black | `Luma | `Freeze | `Diff | `Blocky -> `Video
  | `Shortt | `Moment -> `Audio

let typ_to_string : typ -> string = function
  | `Black -> "Чёрный кадр"
  | `Luma -> "Средняя яркость"
  | `Freeze -> "Заморозка видео"
  | `Diff -> "Средняя разность"
  | `Blocky -> "Блочность"
  | `Shortt -> "Громкость (short term)"
  | `Moment -> "Громкость (momentary)"

let typ_to_unit_string : typ -> string = function
  | `Black | `Freeze | `Blocky -> "%"
  | `Luma | `Diff -> ""
  | `Shortt | `Moment -> "LUFS"

let make_x_axis ?(id = "x-axis") (config : widget_config) : Chartjs.timeAxis Js.t =
  let open Chartjs in
  let duration = int_of_float @@ (Ptime.Span.to_float_s config.duration *. 1000.) in
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
  let streaming = Chartjs_streaming.empty_streaming_config () in
  streaming##.delay := 2000;
  streaming##.duration := duration;
  Chartjs_streaming.set_to_axis axis streaming;
  axis

let make_y_axis ?(id = "y-axis") (config : widget_config) : Chartjs.linearAxis Js.t =
  let open Chartjs in
  let min, max = get_suggested_range config.typ in
  let unit = typ_to_unit_string config.typ in
  let typ = typ_to_string config.typ in
  let label =
    match unit with
    | "" -> typ
    | unit -> typ ^ ", " ^ unit
  in
  let scale_label = empty_scale_label () in
  scale_label##.display := Js._true;
  scale_label##.labelString := Js.string label;
  let ticks = empty_linear_ticks () in
  ticks##.suggestedMin := Js.def min;
  ticks##.suggestedMax := Js.def max;
  let axis = empty_linear_axis () in
  axis##.id := Js.string id;
  axis##.ticks := ticks;
  axis##.scaleLabel := scale_label;
  axis##.position := Position.left;
  axis

let make_options ~x_axes ~y_axes =
  let open Chartjs in
  let tooltips = empty_tooltip () in
  let scales = empty_line_scales () in
  let animation = empty_animation () in
  let hover = empty_hover () in
  let options = empty_line_options () in
  let plugins = Js.Unsafe.obj [||] in
  plugins##.datalabels := Js._false;
  animation##.duration := 0;
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

let make_datasets init (sources : data_source list) (structures : Structure.Annotated.t)
    =
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
      let data = convert_data config init in
      make_datasets data config.sources structures

    val mutable chart = None

    inherit Widget.t elt () as super

    method! init () : unit =
      let x_axis = make_x_axis config in
      let y_axis = make_y_axis config in
      let options = make_options ~x_axes:[x_axis] ~y_axes:[y_axis] in
      let data = Chartjs.empty_data () in
      data##.datasets := Js.array @@ Array.of_list @@ List.map snd datasets;
      chart <- Some (Chartjs.chart_from_canvas Chartjs.Chart.line data options canvas);
      super#init ()

    method! destroy () : unit =
      Option.iter (fun x -> x##destroy) chart;
      super#destroy ()

    method chart : Chartjs.lineChart Js.t =
      match chart with
      | None -> raise Not_found
      | Some x -> x

    method notify : event -> unit =
      function
      (* TODO add structures and state update *)
      | `Data data -> (
        match convert_data config (filter_data config.typ data) with
        | [] -> ()
        | data ->
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
                        self#chart##.data##.datasets##push
                          (Chartjs.coerce_dataset @@ snd ds)
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
              data;
            let config = Chartjs_streaming.empty_update_config () in
            config##.preservation := Js._true;
            self#chart##update_withConfig config)

    (* Private methods *)
    method private update_structures (structures : Structure.Annotated.t) : unit =
      List.iter
        (fun (src, (ds : _ Chartjs.lineDataset Js.t)) ->
          let label = data_source_to_string structures src in
          ds##.label := Js.string label)
        datasets
  end

let make init structures config =
  let elt =
    Js_of_ocaml_tyxml.Tyxml_js.Html.(
      Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      @@ div ~a:[a_class [CSS.root]] [canvas []])
  in
  new t init structures config elt
