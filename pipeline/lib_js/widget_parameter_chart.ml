open Js_of_ocaml
open Containers
open Components
open Common
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

module Point = struct
  open Chartjs.Types
  include Chartjs.Line.Dataset.Make_point(Time)(Float)
end
module Dataset = Chartjs.Line.Dataset.Make(Point)

type data = (data_source * Point.t list) list

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
       List.fold_while (fun _ (x : service_filter) ->
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
      (d : Video_data.t) : data =
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
    let (point : Point.t) =
      { x = error.timestamp
      ; y = error.params.avg
      } in
    [src, [point]]

let convert_audio_data (config : widget_config)
      (d : Audio_data.t) : data =
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
    let (point : Point.t) =
      { x = error.timestamp
      ; y = error.params.avg
      } in
    [src, [point]]

let data_source_to_string (structures : Structure.packed list)
      (src : data_source) : string =
  let open Structure in
  match List.find_map (fun ({ structure = x; _ } : packed) ->
            if Stream.ID.equal x.id src.stream
            then Some x else None) structures with
  | None -> ""
  | Some { channels; _ } ->
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

let make_x_axis ?(id = "x-axis") (config : widget_config) : Chartjs.Scales.t =
  let open Chartjs in
  let open Chartjs_streaming in
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
      ~type_:axis_type
      () in
  let streaming = make ~duration () in
  Per_axis.set axis streaming;
  axis

let make_y_axis ?(id = "y-axis") (config : widget_config) : Chartjs.Scales.t =
  let open Chartjs in
  let open Chartjs.Scales.Cartesian in
  let (min, max) = get_suggested_range config.typ in
  let scale_label =
    Scales.Scale_label.make
      ~display:true
      ~label_string:(typ_to_unit_string config.typ)
      () in
  let ticks =
    Linear.Ticks.make
      ~suggested_min:min
      ~suggested_max:max
      () in
  Linear.make
    ~id
    ~ticks
    ~scale_label
    ~position:`Left
    ()

let make_options ~x_axes ~y_axes : Chartjs.Options.t =
  let scales = Chartjs.Scales.make ~x_axes ~y_axes () in
  let options =
    Chartjs.Options.make
      ~scales
      ~responsive_animation_duration:0
      ~maintain_aspect_ratio:false
      ~responsive:true
      () in
  (* options#animation#set_duration 0;
   * options#hover#set_animation_duration 0; *)
  options

let make_dataset id src structures data =
  let (r, g, b) = colors.(id) in
  let color = Color.to_hexstring @@ Color.of_rgb r g b in
  (* TODO implement label update on structure update *)
  let label = data_source_to_string structures src in
  let ds =
    Dataset.make
      ~data
      ~label
      ~line_tension:0.
      ~point_radius:(`Single 2)
      ~fill:`Off
      ~background_color:color
      ~border_color:color
      () in
  src, ds

let make_datasets (init : data)
      (sources : data_source list)
      (structures : Structure.packed list)
    : (data_source * Dataset.t) list =
  let map id (src : data_source) =
    let data =
      List.find_map (fun (src', data) ->
          if equal_data_source src src'
          then Some data else None) init
      |> Option.get_or ~default:[] in
    make_dataset id src structures data in
  List.mapi map sources

class t ~(init : 'a list)
        ~(structures : Structure.packed list React.signal)
        ~(config : widget_config)
        () =
  let x_axis = make_x_axis config in
  let y_axis = make_y_axis config in
  let (options : Chartjs.Options.t) =
    make_options ~x_axes:[x_axis] ~y_axes:[y_axis] in
  let datasets = make_datasets init config.sources
                   (React.S.value structures) in
  let data = Chartjs.Data.make ~datasets:(List.map snd datasets) () in
  let canvas = Dom_html.(createCanvas document) in
  let chart = Chartjs.make ~options ~data `Line (`Canvas canvas) in
  object(self)

    val mutable _datasets = datasets

    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      super#add_class base_class;
      super#append_child @@ Widget.create canvas;
      (* FIXME maybe remove this map and call 'udpdate'
       * from the top level if needed? *)
      React.S.map ~eq:Equal.unit self#update_structures structures
      |> self#_keep_s

    method append_data (data : data) : unit =
      List.iter (fun (src, (data : Point.t list)) ->
          match List.Assoc.get ~eq:equal_data_source src _datasets with
          | None ->
             begin match config.sources with
             | [] ->
                let id = List.length _datasets in
                let structs = React.S.value structures in
                let ds = make_dataset id src structs data in
                _datasets <- ds :: _datasets;
                let data' = Chartjs.data chart in
                let datasets = Chartjs.Data.datasets data' in
                let (_ : int) = Chartjs.Data.Datasets.push datasets [snd ds] in
                Chartjs.update chart None
             | _ -> ()
             end
          | Some (ds : Dataset.t) ->
             List.iter (fun (point : Point.t) ->
                 let data = Dataset.data ds in
                 let (_ : int) = Dataset.Values.push data [point] in
                 ()) data;
             Chartjs.update chart None) data

    (* Private methods *)

    method private update_structures (structures : Structure.packed list) : unit =
      List.iter (fun (src, (ds : Dataset.t)) ->
          let label = data_source_to_string structures src in
          Dataset.set_label ds label) _datasets

  end

let make_dashboard_item ~init ~structures ~config () =
  let widget = new t ~init ~structures ~config () in
  Dashboard.Item.make_item
    ~name:(typ_to_string config.typ)
    widget
