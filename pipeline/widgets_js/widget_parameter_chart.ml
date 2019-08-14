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
  ; filter : data_filter list
  ; settings : widget_settings option
  }
and widget_settings =
  { range : (float * float) option
  }
and data_filter =
  { stream_id : Stream.ID.t
  ; services : service_filter list
  }
and service_filter =
  { service_id : int
  ; pids : int list
  }
and data_source =
  { stream : Stream.ID.t
  ; service : int
  ; pid : int
  } [@@deriving eq, yojson]
and typ =
  [ `Black
  | `Luma
  | `Freeze
  | `Diff
  | `Blocky
  | `Shortt
  | `Moment
  ]

type event =
  [ `Data of (data_source * kind) list
  ]
and kind =
  [ `Video of Qoe_errors.Video_data.data
  | `Audio of Qoe_errors.Audio_data.data
  ]

let filter_data typ (data : (data_source * kind) list) =
  Utils.List.filter_map (fun (src, kind) ->
      match typ, (kind : kind) with
      | `Black, `Video x -> Some (src, x.black)
      | `Luma, `Video x -> Some (src, x.luma)
      | `Freeze, `Video x -> Some (src, x.freeze)
      | `Diff, `Video x -> Some (src, x.diff)
      | `Blocky, `Video x -> Some (src, x.blocky)
      | `Shortt, `Audio x -> Some (src, x.shortt)
      | `Moment, `Audio x -> Some (src, x.moment)
      | _ -> None) data

let colors =
  Random.init 255;
  Array.init 100 (fun _ ->
      (Random.int 255),
      (Random.int 255),
      (Random.int 255))

let get_suggested_range : typ -> float * float = function
  | `Black -> 0.0, 100.0
  | `Luma -> 16.0, 235.0
  | `Freeze -> 0.0, 100.0
  | `Diff -> 0.0, 216.0
  | `Blocky -> 0.0, 100.0
  | `Shortt | `Moment -> -40., 0.

let filter (src : data_source) (filter : data_filter list) : bool =
  let check_pid pid = function
    | [] -> true
    | pids -> List.mem pid pids in
  let check_service service pid = function
    | [] -> true
    | services ->
      Utils.List.fold_while (fun _ (x : service_filter) ->
          if service = x.service_id
          then check_pid pid x.pids, `Stop
          else false, `Continue) false services in
  let { stream; service; pid } = src in
  let rec aux = function
    | [] -> false
    | (hd : data_filter) :: tl ->
      if Stream.ID.equal hd.stream_id stream
      then check_service service pid hd.services
      else aux tl in
  match filter with
  | [] -> true
  | filter -> aux filter

let convert_data
    (config : widget_config)
    (d : (data_source * (Qoe_errors.point array)) list) =
  let data =
    List.fold_left (fun acc (src, points) ->
        if not (filter src config.filter)
        then acc
        else Utils.List.Assoc.update ~eq:equal_data_source
            (function
              | None -> Some points
              | Some l -> Some (Array.append l points))
            src acc) [] d in
  List.map (fun (src, points) ->
      Array.sort (fun (a : Qoe_errors.point) b ->
        Ptime.compare a.time b.time) points;
      src, Array.map (fun (x : Qoe_errors.point) ->
          print_endline @@ Ptime.to_rfc3339 x.time;
          Chartjs.createDataPoint
            ~x:(Chartjs.Time.of_float_s @@ Ptime.to_float_s x.time)
            ~y:x.data)
        points) data

let data_source_to_string (structures : Structure.Annotated.t)
    (src : data_source) : string =
  let open Structure in
  match Utils.List.find_map (fun (_, (x : Annotated.structure)) ->
      if Stream.ID.equal x.id src.stream
      then Some x else None) structures with
  | None -> ""
  | Some { channels; _ } ->
    begin match List.find_opt (fun (_, (x : Annotated.channel)) ->
        src.service = x.number) channels with
    | None -> ""
    | Some (_, channel) ->
      begin match List.find_opt (fun (_, (x : pid)) ->
          x.pid = src.pid) channel.pids with
      | None -> ""
      | Some (_, pid) ->
        Printf.sprintf "%s. PID %d (%s)"
          channel.service_name
          pid.pid
          pid.stream_type_name
      end
    end

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

let make_x_axis ?(id = "x-axis") (config : widget_config)
  : Chartjs.timeCartesianAxis Js.t =
  let open Chartjs in
  let duration =
    int_of_float
    @@ Ptime.Span.to_float_s config.duration *. 1000. in
  let scale_label = createScaleLabel () in
  scale_label##.display := Js._true;
  scale_label##.labelString := Js.string "Время";
  let time_format = "HH:mm:ss" in
  let display_formats = createTimeDisplayFormats () in
  display_formats##.second := Js.string time_format;
  display_formats##.minute := Js.string time_format;
  display_formats##.hour := Js.string time_format;
  let time_options = createTimeCartesianOptions () in
  time_options##.isoWeekday := Js._true;
  time_options##.displayFormats := display_formats;
  time_options##.tooltipFormat := Js.string "ll HH:mm:ss";
  let ticks = createTimeCartesianTicks () in
  ticks##.autoSkipPadding := 2;
  let axis = createTimeCartesianAxis () in
  axis##.id := Js.string id;
  axis##.scaleLabel := scale_label;
  axis##.ticks := ticks;
  axis##.time := time_options;
  axis##.position := Position.bottom;
  axis##._type := Js.string "realtime";
  let streaming = Chartjs_streaming.create
      ~duration
      () in
  Chartjs_streaming.set_per_axis axis streaming;
  axis

let make_y_axis ?(id = "y-axis") (config : widget_config)
  : Chartjs.linearCartesianAxis Js.t =
  let open Chartjs in
  let (min, max) = get_suggested_range config.typ in
  let scale_label = createScaleLabel () in
  scale_label##.display := Js._true;
  scale_label##.labelString := Js.string @@ typ_to_unit_string config.typ;
  let ticks = createLinearCartesianTicks () in
  ticks##.suggestedMin := min;
  ticks##.suggestedMax := max;
  let axis = createLinearCartesianAxis () in
  axis##.id := Js.string id;
  axis##.ticks := ticks;
  axis##.scaleLabel := scale_label;
  axis##.position := Position.left;
  axis

let make_options ~xAxes ~yAxes =
  let open Chartjs in
  let scales = createLineScales ~xAxes ~yAxes () in
  let options = createLineOptions () in
  options##.scales := scales;
  options##.responsiveAnimationDuration := 0;
  options##.maintainAspectRatio := Js._false;
  options##.responsive := Js._true;
  options

let make_dataset id src structures data =
  let (r, g, b) = colors.(id) in
  let color = Color.to_hexstring @@ Color.of_rgb r g b in
  (* TODO implement label update on structure update *)
  let label = data_source_to_string structures src in
  let ds = Chartjs.createLineDataset @@ Js.array data in
  ds##.label := Js.string label;
  ds##.lineTension := 0.;
  ds##.pointRadius := Chartjs.Scriptable_indexable.of_single 2;
  ds##.backgroundColor := Chartjs.Color.of_string color;
  ds##.borderColor := Chartjs.Color.of_string color;
  src, ds

let make_datasets init
    (sources : data_source list)
    (structures : Structure.Annotated.t) =
  let map id (src : data_source) =
    let data =
      Utils.List.find_map (fun (src', data) ->
          if equal_data_source src src'
          then Some data else None) init
      |> function None -> [||] | Some x -> x in
    make_dataset id src structures data in
  List.mapi map sources

class t
    (init : 'a list)
    (structures : Structure.Annotated.t)
    (config : widget_config)
    (elt : Dom_html.element Js.t) = object(self)

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
    let options = make_options ~xAxes:[x_axis] ~yAxes:[y_axis] in
    let data = Chartjs.createData ~datasets:(List.map snd datasets) () in
    chart <- Some (Chartjs.chart_from_canvas Chartjs.Chart.line data options canvas);
    super#init ()

  method! destroy () : unit =
    Utils.Option.iter (fun x -> x##destroy) chart;
    super#destroy ()

  method chart : Chartjs.lineChart Js.t =
    match chart with
    | None -> raise Not_found
    | Some x -> x

  method notify : event -> unit = function
    (* TODO add structures and state update *)
    | `Data data ->
      match convert_data config (filter_data config.typ data) with
      | [] -> ()
      | data ->
        List.iter (fun (src, data) ->
            match Utils.List.Assoc.get ~eq:equal_data_source src datasets with
            | None ->
              (match config.sources with
               | [] ->
                 let id = List.length datasets in
                 let ds = make_dataset id src structures data in
                 datasets <- ds :: datasets;
                 let (_ : int) = self#chart##.data##.datasets##push
                     (Chartjs.coerce_dataset @@ snd ds) in
                 ()
               | _ -> ())
            | Some (ds : _ Chartjs.lineDataset Js.t) ->
              let data = ds##.data##concat (Js.array data) in
              ds##.data := data) data;
        let update_config = Chartjs_streaming.createUpdateConfig () in
        update_config##.preservation := Js._true;
        self#chart##update_withConfig update_config

  (* Private methods *)

  method private update_structures (structures : Structure.Annotated.t) : unit =
    List.iter (fun (src, (ds : _ Chartjs.lineDataset Js.t)) ->
        let label = data_source_to_string structures src in
        ds##.label := Js.string label) datasets
end

let make init structures config =
  let elt = Js_of_ocaml_tyxml.Tyxml_js.Html.(
      Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
      @@ div ~a:[a_class [CSS.root]] [canvas []]) in
  new t init structures config elt

