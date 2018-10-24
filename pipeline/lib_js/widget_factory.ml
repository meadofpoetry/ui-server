open Containers
open Components
open Qoe_errors
open Common
open Ui_templates.Factory

type item =
  | Chart of Widget_parameter_chart.widget_config option

class t () =
object(self)

  val mutable _video_data : Video_data.t React.event State.t option = None

  method create : item -> Widget.t Dashboard.Item.item = function
    | Chart cfg ->
       let open Widget_parameter_chart in
       let video_data = self#get_video_data () in
       let config = Option.get_exn cfg in
       let item = make_dashboard_item ~init:[] ~config () in
       React.E.map (fun data ->
           let data = convert_video_data config data in
           item.widget#append_data data) video_data
       |> React.E.keep;
       { item with widget = item.widget#widget }

  method destroy () : unit =
    ()

  method available : Dashboard.available =
    `List []

  method serialize (x : item) : Yojson.Safe.json =
    `Null

  method deserialize (json : Yojson.Safe.json)
         : (item, string) result =
    Ok (Chart None)

  (* Private methods *)

  method private get_video_data () = match _video_data with
    | Some state -> state.value
    | None ->
       let e, sock = Requests_measurements.WS.get_video () in
       let fin = fun () -> sock##close; React.E.stop ~strong:true e in
       let state = State.make ~fin e in
       _video_data <- Some state;
       e

  method private get_audio_date () =
    []

end
