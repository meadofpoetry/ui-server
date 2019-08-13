(* open Js_of_ocaml
 * open Components
 * open Application_types
 * open Board_niitv_dvb_types
 * open Widget_types
 * 
 * let ( % ) f g x = f (g x)
 * 
 * module CSS = struct
 *   let root = "niitv-dvb-measurements-chart"
 * end
 * 
 * module type S = sig
 *   type t
 *   val equal : t -> t -> bool
 *   val to_string : t -> string
 *   val to_yojson : t -> Yojson.Safe.t
 *   val of_yojson : Yojson.Safe.t -> (t, string) result
 * end *)

(* module Point = struct
 *   open Chartjs.Types
 *   include Chartjs.Line.Dataset.Make_point(Time)(Float)
 * end
 * 
 * module Dataset = Chartjs.Line.Dataset.Make(Point)
 * 
 * module Make(S : S) = struct
 * 
 *   type data = (S.t * Point.t list) list
 * 
 *   type event =
 *     [ `Data of (S.t * Measure.t ts list) list
 *     | `State of Topology.state
 *     ]
 * 
 *   type widget_config =
 *     { sources : S.t list
 *     ; typ : measure_type
 *     ; settings : widget_settings
 *     }
 *   and widget_settings =
 *     { range : (float * float) option
 *     ; period : period
 *     }
 *   and period =
 *     [ `Realtime of Time.Period.t
 *     | `Archive of Time.Range.t
 *     ] [@@deriving yojson, eq]
 * 
 *   let colors =
 *     Random.init 255;
 *     Array.init 100 (fun _ ->
 *         Random.int 255,
 *         Random.int 255,
 *         Random.int 255)
 * 
 *   let duration_of_period : period -> Time.Period.t = function
 *     | `Realtime x -> x
 *     | `Archive (_, x) -> x
 * 
 *   let get_suggested_range = function
 *     | `Power -> (-70.0, 0.0)
 *     | `Mer -> (0.0, 45.0)
 *     | `Ber -> (0.0, 0.00001)
 *     | `Freq -> (-10.0, 10.0)
 *     | `Bitrate -> (0.0, 1.0)
 * 
 *   let format_value (v : float) (config : widget_config) : string =
 *     let unit = measure_type_to_unit config.typ in
 *     let v = match config.typ with
 *       | `Mer -> Printf.sprintf "%g" v
 *       | `Ber -> Printf.sprintf "%0.3e" v
 *       | `Freq -> Printf.sprintf "%g" v
 *       | `Power -> Printf.sprintf "%g" v
 *       | `Bitrate -> Printf.sprintf "%g" v in
 *     Printf.sprintf "%s %s" v unit
 * 
 *   let make_y_axis ?(id = "y-axis") (config : widget_config) =
 *     let open Chartjs.Scales.Cartesian in
 *     let range = get_suggested_range config.typ in
 *     let scale_label =
 *       Chartjs.Scales.Scale_label.make
 *         ~display:true
 *         ~label_string:(measure_type_to_unit config.typ)
 *         () in
 *     let position = `Left in
 *     let axis, set_range = match config.typ with
 *       | `Ber ->
 *         let axis = Logarithmic.make ~id ~scale_label ~position () in
 *         let set_range = function
 *           | None ->
 *             let open Logarithmic in
 *             let ticks = ticks axis in
 *             Ticks.set_min ticks None;
 *             Ticks.set_max ticks None
 *           | Some (min, max) ->
 *             let open Logarithmic in
 *             let ticks = ticks axis in
 *             Ticks.set_max ticks (Some max);
 *             Ticks.set_min ticks (Some min) in
 *         axis, set_range
 *       | _ ->
 *         let ticks =
 *           Linear.Ticks.make
 *             ~suggested_min:(fst range)
 *             ~suggested_max:(snd range)
 *             () in
 *         let axis = Linear.make ~id ~ticks ~scale_label ~position () in
 *         let set_range = function
 *           | None ->
 *             let open Linear in
 *             let ticks = ticks axis in
 *             Ticks.set_min ticks None;
 *             Ticks.set_max ticks None
 *           | Some (min, max) ->
 *             let open Linear in
 *             let ticks = ticks axis in
 *             Ticks.set_max ticks (Some max);
 *             Ticks.set_min ticks (Some min) in
 *         axis, set_range in
 *     axis, set_range
 * 
 *   let make_x_axis ?(id = "x-axis") (config : widget_config) : Chartjs.Scales.t =
 *     let scale_label =
 *       Chartjs.Scales.Scale_label.make
 *         ~display:true
 *         ~label_string:"Время"
 *         () in
 *     let display_formats =
 *       Chartjs.Scales.Cartesian.Time.Time.Display_formats.make
 *         ~minute:"HH:mm:ss"
 *         ~second:"HH:mm:ss"
 *         ~hour:"HH:mm:ss"
 *         () in
 *     let time =
 *       Chartjs.Scales.Cartesian.Time.Time.make
 *         ~iso_weekday:true
 *         ~display_formats
 *         ~tooltip_format:"ll HH:mm:ss"
 *         () in
 *     let ticks =
 *       Chartjs.Scales.Cartesian.Time.Ticks.make
 *         ~auto_skip_padding:2
 *         () in
 *     let axis_type = match config.settings.period with
 *       | `Realtime _ -> Chartjs_streaming.axis_type
 *       | `Archive _ -> `Time in
 *     let axis =
 *       Chartjs.Scales.Cartesian.Time.make
 *         ~id
 *         ~scale_label
 *         ~ticks
 *         ~time
 *         ~position:`Bottom
 *         ~type_:axis_type
 *         () in
 *     axis
 * 
 *   let make_options ~x_axes ~y_axes
 *       (config : widget_config) : Chartjs.Options.t =
 *     let open Chartjs in
 *     let scales = Scales.make ~x_axes ~y_axes () in
 *     let legend = Options.Legend.make ~display:false () in
 *     let plugins = Options.Plugins.make () in
 *     let callbacks =
 *       Options.Tooltips.Callbacks.make
 *         ~label:(fun item data ->
 *             let ds_index = item.dataset_index in
 *             let datasets = Data.datasets data in
 *             let dataset = Data.Datasets.(datasets.%[ds_index]) in
 *             let label = Dataset.label dataset in
 *             let data = Dataset.data dataset in
 *             let value = Dataset.Values.(data.%[item.index]) in
 *             Printf.sprintf "%s: %s" label (format_value value.y config))
 *         () in
 *     let tooltips =
 *       Options.Tooltips.make
 *         ~callbacks
 *         ~mode:`Index
 *         ~intersect:false
 *         () in
 *     (match config.settings.period with
 *      | `Realtime period ->
 *        let duration =
 *          int_of_float
 *          @@ Float.mul 1000.
 *          @@ Ptime.Span.to_float_s period in
 *        let streaming = Chartjs_streaming.make
 *            ~delay:3000
 *            ~duration
 *            () in
 *        Chartjs_streaming.Per_chart.set plugins (Some streaming);
 *      | `Archive _ -> ());
 *     Chartjs_datalabels.Per_chart.set plugins None;
 *     Options.make
 *       ~scales
 *       ~plugins
 *       ~legend
 *       ~tooltips
 *       ~maintain_aspect_ratio:false
 *       ~responsive:true
 *       ~responsive_animation_duration:0
 *       ()
 * 
 *   let make_dataset id src (data : Point.t list) =
 *     let data = List.sort (fun (a : Point.t) b -> Ptime.compare a.x b.x) data in
 *     let label = Printf.sprintf "%s %s" module_name (S.to_string src) in
 *     let (r, g, b) = colors.(id) in
 *     let color = Color.to_hexstring @@ Color.of_rgb r g b in
 *     let ds =
 *       Dataset.make
 *         ~data
 *         ~label
 *         ~fill:`Off
 *         ~point_radius:(`Single 2)
 *         ~line_tension:0.
 *         ~background_color:color
 *         ~border_color:color
 *         ~cubic_interpolation_mode:`Monotone
 *         () in
 *     src, ds
 * 
 *   let make_datasets (init : data) (sources : S.t list) =
 *     let map id (src : S.t) =
 *       let data =
 *         Utils.List.find_map (fun (src', data) ->
 *             if S.equal src src'
 *             then Some data else None) init
 *         |> function None -> [] | Some x -> x in
 *       make_dataset id src data in
 *     List.mapi map sources
 * 
 *   let get_power (m : Measure.t) = m.power
 * 
 *   let get_mer (m : Measure.t) = m.mer
 * 
 *   let get_ber (m : Measure.t) = m.ber
 * 
 *   let get_freq (m : Measure.t) = match m.freq with
 *     | None -> None
 *     | Some x -> Some (float_of_int x)
 * 
 *   let get_bitrate (m : Measure.t) = match m.bitrate with
 *     | None -> None
 *     | Some x -> Some (float_of_int x /. 1_000_000.)
 * 
 *   let data_of_init (get : Measure.t -> float option)
 *       (init : (S.t * Measure.t ts list) list) : data =
 *     List.map (fun ((id : S.t), meas) ->
 *         let data =
 *           List.map (fun { data; timestamp } ->
 *               let y = match get data with None -> nan | Some x -> x in
 *               ({ x = timestamp; y } : Point.t)) meas in
 *         id, data) init
 * 
 *   let getter = function
 *     | `Power -> get_power
 *     | `Mer -> get_mer
 *     | `Ber -> get_ber
 *     | `Freq -> get_freq
 *     | `Bitrate -> get_bitrate
 * 
 *   class t ~init (config : widget_config) (elt : Dom_html.element Js.t) = object(self)
 * 
 *     val canvas : Dom_html.canvasElement Js.t =
 *       Js.Unsafe.coerce @@ Element.query_selector_exn elt "canvas"
 * 
 *     val mutable datasets = make_datasets
 *         (data_of_init (getter config.typ) init)
 *         config.sources
 * 
 *     val mutable chart = None
 * 
 *     inherit Widget.t elt () as super
 * 
 *     method! init () : unit =
 *       let x_axis = make_x_axis config in
 *       let y_axis, _ = make_y_axis config in
 *       let options = make_options ~x_axes:[x_axis] ~y_axes:[y_axis] config in
 *       let data = Chartjs.Data.make ~datasets:(List.map snd datasets) () in
 *       chart <- Some (Chartjs.make ~options ~data `Line (`Canvas canvas));
 *       Js.Unsafe.global##.console##log self#chart |> ignore;
 *       super#init ()
 * 
 *     method! destroy () : unit =
 *       Utils.Option.iter Chartjs.destroy chart;
 *       super#destroy ()
 * 
 *     method chart : Chartjs.t =
 *       match chart with
 *       | None -> raise Not_found
 *       | Some x -> x
 * 
 *     method notify : event -> unit = function
 *       | `State _state ->
 *         (\* TODO the idea is to insert `null` value after device state changed. *\)
 *         ()
 *       | `Data data ->
 *         let data = data_of_init (getter config.typ) data in
 *         List.iter (fun ((s : S.t), data) ->
 *             match Utils.List.Assoc.get ~eq:S.equal s datasets with
 *             | None ->
 *               let id = List.length datasets in
 *               let dataset = make_dataset id s data in
 *               datasets <- dataset :: datasets;
 *               Chartjs.Data.set_datasets
 *                 (Chartjs.data self#chart)
 *                 (List.map snd datasets);
 *               Chartjs.update self#chart
 *                 (Some (Chartjs_streaming.make_config
 *                          ~preservation:true
 *                          ()))
 *             | Some ds ->
 *               let data' = Dataset.data ds in
 *               let push v = Dataset.(Values.push data' [v]) in
 *               List.iter (ignore % push) data;
 *               Chartjs.update self#chart
 *                 (Some (Chartjs_streaming.make_config
 *                          ~preservation:true
 *                          ()))) data
 *   end
 * 
 *   let make (init : (S.t * Measure.t ts list) list)
 *       (config : widget_config) : t =
 *     let elt = Js_of_ocaml_tyxml.Tyxml_js.Html.(
 *         Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element
 *         @@ div ~a:[a_class [CSS.root]] [canvas []]) in
 *     new t ~init config elt
 * 
 * end *)
