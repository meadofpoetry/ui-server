open Js_of_ocaml
open Components
open Application_types
open Board_niitv_dvb_types
open Widget_types

type event =
  [ `Data of (int * Measure.t ts list) list
  | `State of Topology.state
  ]

type widget_config =
  { sources : data_source list
  ; typ : measure_type
  ; period : period
  ; settings : widget_settings option
  }
and period =
  [ `Realtime of Time.Period.t
  | `Archive of Time.Range.t
  ]
and widget_settings =
  { range : (float * float) option
  }
and data_source = Stream.ID.t [@@deriving yojson, eq]

let duration_of_config ({ period; _ } : widget_config) : Time.Period.t =
  match period with
  | `Realtime x -> x
  | `Archive (_, x) -> x

let get_suggested_range = function
  | `Power -> (-70.0, 0.0)
  | `Mer -> (0.0, 45.0)
  | `Ber -> (0.0, 0.00001)
  | `Freq -> (-10.0, 10.0)
  | `Bitrate -> (0.0, 1.0)

let make_x_axis ?(id = "x-axis") (config : widget_config) : Chartjs.Scales.t =
  let open Chartjs in
  let open Chartjs_streaming in
  let duration =
    int_of_float
    @@ Ptime.Span.to_float_s (duration_of_config config) *. 1000. in
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

class t (elt : Dom_html.element Js.t) = object
  inherit Widget.t elt ()

  method notify : event -> unit = function
    | `State _state -> ()
    | `Data _data -> ()
end
