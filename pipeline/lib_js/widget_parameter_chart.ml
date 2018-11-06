open Containers
open Components
open Common
open Chartjs
open Qoe_errors

let base_class = "pipeline-chart"

type widget_config =
  { duration : Time.Period.t
  ; typ : labels
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

type 'a point = (Time.t, 'a) Chartjs.Line.point

type 'a data = (data_source * ('a point list)) list

type dataset = (Time.t, float) Line.Dataset.t

let colors =
  Random.init 255;
  let st = Random.get_state () in
  Array.init 100 (fun _ ->
      Random.run ~st (Random.int 255),
      Random.run ~st (Random.int 255),
      Random.run ~st (Random.int 255))

let get_suggested_range = function
  | `Black -> 0.0, 100.0
  | `Luma -> 16.0, 235.0
  | `Freeze -> 0.0, 100.0
  | `Diff -> 0.0, 216.0
  | `Blocky -> 0.0, 100.0
  | _ -> -40., 0.

let filter (src : data_source) (filter : data_filter list) : bool =
  let check_pid pid = function
    | [] -> true
    | pids -> List.mem ~eq:Int.equal pid pids in
  let check_service service pid = function
    | [] -> true
    | services ->
       List.fold_while (fun acc (x : service_filter) ->
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

let convert_video_data (config : widget_config)
      (d : Video_data.t) : 'a data =
  let (src : data_source) =
    { stream = d.stream
    ; service = d.channel
    ; pid = d.pid
    } in
  if not (filter src config.filter) then [] else
    let (error : error) = match config.typ with
      | `Black -> d.errors.black
      | `Luma -> d.errors.luma
      | `Freeze -> d.errors.freeze
      | `Diff -> d.errors.diff
      | `Blocky -> d.errors.blocky
      | `Silence_shortt | `Silence_moment | `Loudness_shortt | `Loudness_moment ->
         failwith "not a video chart" in
    let (point : float point) =
      { x = error.timestamp
      ; y = error.params.avg
      } in
    [src, [point]]

let convert_audio_data (config : widget_config)
      (d : Audio_data.t) : 'a data =
  let (src : data_source) =
    { stream = d.stream
    ; service = d.channel
    ; pid = d.pid
    } in
  if not (filter src config.filter) then [] else
    let (error : error) = match config.typ with
      | `Silence_shortt -> d.errors.silence_shortt
      | `Silence_moment -> d.errors.silence_moment
      | `Loudness_shortt -> d.errors.loudness_shortt
      | `Loudness_moment -> d.errors.loudness_moment
      | _ -> failwith "not an audio chart" in
    let (point : float point) =
      { x = error.timestamp
      ; y = error.params.avg
      } in
    [src, [point]]

let data_source_to_string (structures : Structure.t list)
      (src : data_source) : string =
  let open Structure in
  match List.find_opt (fun (x : t) ->
            Stream.ID.equal x.source.id src.stream) structures with
  | None -> ""
  | Some { structure = { channels; _ }; source } ->
     begin match List.find_opt (fun (x : channel) ->
                     src.service = x.number) channels with
     | None -> ""
     | Some channel ->
        begin match List.find_opt (fun (x : pid) ->
                        x.pid = src.pid) channel.pids with
        | None -> ""
        | Some pid ->
           Printf.sprintf "%s. PID %d (%s)"
             channel.service_name
             pid.pid
             pid.stream_type_name
        end
     end

let typ_to_content : labels -> [`Video | `Audio] = function
  | `Black | `Luma | `Freeze | `Diff | `Blocky -> `Video
  | `Silence_shortt | `Silence_moment | `Loudness_shortt | `Loudness_moment ->
     `Audio

let typ_to_string : labels -> string = function
  | `Black -> "Чёрный кадр"
  | `Luma -> "Средняя яркость"
  | `Freeze -> "Заморозка видео"
  | `Diff -> "Средняя разность"
  | `Blocky -> "Блочность"
  | `Silence_shortt | `Loudness_shortt -> "Громкость (short term)"
  | `Silence_moment | `Loudness_moment -> "Громкость (momentary)"

let typ_to_unit_string : labels -> string = function
  | `Black | `Freeze | `Blocky -> "%"
  | `Luma | `Diff -> ""
  | (`Silence_shortt | `Silence_moment
    | `Loudness_shortt | `Loudness_moment) -> "LUFS"

let make_x_axis ?(id = "x-axis") (config : widget_config)
    : (Time.t, Time.span) Line.Axes.Time.t =
  let delta = config.duration in
  let axis =
    new Line.Axes.Time.t
      ~id
      ~delta
      ~position:`Bottom
      ~typ:Ptime
      () in
  axis#scale_label#set_display true;
  axis#scale_label#set_label_string "Время";
  axis#time#set_tooltip_format "ll HH:mm:ss";
  axis#ticks#set_auto_skip_padding 2;
  axis

let make_y_axis ?(id = "y-axis") (config : widget_config)
    : float Line.Axes.Linear.t =
  let axis =
    new Line.Axes.Linear.t
      ~id
      ~position:`Left
      ~typ:Float
      () in
  let (min, max) = get_suggested_range config.typ in
  let label = typ_to_unit_string config.typ in
  axis#ticks#set_suggested_min min;
  axis#ticks#set_suggested_max max;
  axis#scale_label#set_display true;
  axis#scale_label#set_label_string label;
  axis

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
  options#set_maintain_aspect_ratio false;
  options#set_responsive true;
  options#animation#set_duration 0;
  options#hover#set_animation_duration 0;
  options#set_responsive_animation_duration 0;
  options

let make_dataset ~x_axis ~y_axis id src structures data =
  (* TODO implement label update on structure update *)
  let label = data_source_to_string structures src in
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
      (sources : data_source list)
      (structures : Structure.t list)
    : (data_source * dataset) list =
  let map id (src : data_source) =
    let data =
      List.find_map (fun (src', data) ->
          if equal_data_source src src'
          then Some data else None) init
      |> Option.get_or ~default:[] in
    make_dataset ~x_axis ~y_axis id src structures data in
  List.mapi map sources

class t ~(init : float data)
        ~(structures : Structure.t list React.signal)
        ~(config : widget_config)
        () =
  let x_axis = make_x_axis config in
  let y_axis = make_y_axis config in
  let (options : Line.Options.t) =
    make_options ~x_axes:[x_axis] ~y_axes:[y_axis] config in
  let (datasets : (data_source * dataset) list) =
    make_datasets ~x_axis ~y_axis init config.sources
      (React.S.value structures) in
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
      (* FIXME maybe remove this map and call 'udpdate'
       * from the top level if needed? *)
      React.S.map ~eq:Equal.unit self#update_structures structures
      |> self#_keep_s

    method append_data (data : float data) : unit =
      List.iter (fun (src, data) ->
          match List.Assoc.get ~eq:equal_data_source src _datasets with
          | None ->
             begin match config.sources with
             | [] ->
                let id = List.length _datasets in
                let structs = React.S.value structures in
                let ds = make_dataset ~x_axis ~y_axis id src structs data in
                _datasets <- ds :: _datasets;
                chart#set_datasets @@ List.map snd _datasets;
                chart#update None
             | _ -> ()
             end
          | Some ds ->
             List.iter (fun point -> ds#push point) data;
             chart#update None) data

    (* Private methods *)

    method private update_structures (structures : Structure.t list) : unit =
      List.iter (fun (src, ds) ->
          let label = data_source_to_string structures src in
          ds#set_label label) _datasets

  end

let make_dashboard_item ~init ~structures ~config () =
  let widget = new t ~init ~structures ~config () in
  Dashboard.Item.make_item
    ~name:(typ_to_string config.typ)
    widget
