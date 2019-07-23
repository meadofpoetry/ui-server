open Js_of_ocaml
open Components
open Application_types
open Pipeline_types

let base_class = "pipeline-chart"

type widget_config =
  { duration : Time.Period.t
  ; typ : Qoe_errors.labels
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
  Array.init 100 (fun _ ->
      (Random.int 255),
      (Random.int 255),
      (Random.int 255))

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

let convert_video_data (config : widget_config)
      (d : Qoe_errors.Video_data.t list) : data =
  List.fold_left (fun acc (point : Qoe_errors.Video_data.t) ->
      let (src : data_source) =
        { stream = point.stream
        ; service = point.channel
        ; pid = point.pid
        } in
      if not (filter src config.filter) then [] else
        let (error : Qoe_errors.error) = match config.typ with
          | `Black -> point.errors.black
          | `Luma -> point.errors.luma
          | `Freeze -> point.errors.freeze
          | `Diff -> point.errors.diff
          | `Blocky -> point.errors.blocky
          | (`Silence_shortt | `Silence_moment
            | `Loudness_shortt | `Loudness_moment) ->
             failwith "not an audio chart" in
        let (point : Point.t) =
          { x = error.timestamp
          ; y = match config.typ with
                | `Black -> error.params.max
                | `Luma -> error.params.min
                | `Freeze -> error.params.max
                | `Diff -> error.params.min
                | `Blocky -> error.params.min
                | _ -> failwith "not an audio chart"
          } in
        Utils.List.Assoc.update ~eq:equal_data_source
          (function None -> Some [point]
                  | Some l -> Some (point :: l))
          src acc) [] d

let convert_audio_data (config : widget_config)
      (d : Qoe_errors.Audio_data.t list) : data =
  List.fold_left (fun acc (point : Qoe_errors.Audio_data.t) ->
      let (src : data_source) =
        { stream = point.stream
        ; service = point.channel
        ; pid = point.pid
        } in
      if not (filter src config.filter) then [] else
        let (error : Qoe_errors.error) = match config.typ with
          | `Silence_shortt -> point.errors.silence_shortt
          | `Silence_moment -> point.errors.silence_moment
          | `Loudness_shortt -> point.errors.loudness_shortt
          | `Loudness_moment -> point.errors.loudness_moment
          | _ -> failwith "not an audio chart" in
        let (point : Point.t) =
          { x = error.timestamp
          ; y = error.params.avg
          } in
        Utils.List.Assoc.update ~eq:equal_data_source
          (function
            | None -> Some [point]
            | Some l -> Some (point :: l))
          src acc) [] d

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

let typ_to_content : Qoe_errors.labels -> [`Video | `Audio] = function
  | `Black | `Luma | `Freeze | `Diff | `Blocky -> `Video
  | `Silence_shortt | `Silence_moment | `Loudness_shortt | `Loudness_moment ->
     `Audio

let typ_to_string : Qoe_errors.labels -> string = function
  | `Black -> "Чёрный кадр"
  | `Luma -> "Средняя яркость"
  | `Freeze -> "Заморозка видео"
  | `Diff -> "Средняя разность"
  | `Blocky -> "Блочность"
  | `Silence_shortt | `Loudness_shortt -> "Громкость (short term)"
  | `Silence_moment | `Loudness_moment -> "Громкость (momentary)"

let typ_to_unit_string : Qoe_errors.labels -> string = function
  | `Black | `Freeze | `Blocky -> "%"
  | `Luma | `Diff -> ""
  | `Silence_shortt | `Silence_moment | `Loudness_shortt | `Loudness_moment ->
     "LUFS"

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
  let plugins = Chartjs.Options.Plugins.make () in
  Chartjs_datalabels.Per_chart.set plugins None;
  let options =
    Chartjs.Options.make
      ~scales
      ~responsive_animation_duration:0
      ~maintain_aspect_ratio:false
      ~responsive:true
      ~plugins
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
      (structures : Structure.Annotated.t)
    : (data_source * Dataset.t) list =
  let map id (src : data_source) =
    let data =
      Utils.List.find_map (fun (src', data) ->
          if equal_data_source src src'
          then Some data else None) init
      |> function None -> [] | Some x -> x in
    make_dataset id src structures data in
  List.mapi map sources

class t ~(init : 'a list)
        ~(structures : Structure.Annotated.t React.signal)
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
      Lwt_react.S.keep @@ React.S.map ~eq:(=) self#update_structures structures

    method append_data (data : data) : unit =
      List.iter (fun (src, (data : Point.t list)) ->
          let data = List.sort (fun (a : Point.t) (b : Point.t) ->
                         Ptime.compare a.x b.x) data in
          match Utils.List.Assoc.get ~eq:equal_data_source src _datasets with
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
                let config = Chartjs_streaming.make_config ~preservation:true () in
                Chartjs.update chart (Some config)
             | _ -> ()
             end
          | Some (ds : Dataset.t) ->
             let data' = Dataset.data ds in
             List.iter (fun (point : Point.t) ->
                 let (_ : int) = Dataset.Values.push data' [point] in
                 ()) data;
             let config = Chartjs_streaming.make_config ~preservation:true () in
             Chartjs.update chart (Some config)) data

    (* Private methods *)

    method private update_structures (structures : Structure.Annotated.t) : unit =
      List.iter (fun (src, (ds : Dataset.t)) ->
          let label = data_source_to_string structures src in
          Dataset.set_label ds label) _datasets

  end

(* let make_dashboard_item ~init ~structures ~config () =
 *   let widget = new t ~init ~structures ~config () in
 *   Dashboard.Item.make_item
 *     ~name:(typ_to_string config.typ)
 *     widget *)
