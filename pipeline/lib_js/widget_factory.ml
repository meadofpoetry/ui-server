open Containers
open Components
open Qoe_errors
open Ui_templates.Factory
open Lwt_result.Infix
open Common

let ( % ) = Fun.( % )

type item =
  | Chart of Widget_parameter_chart.widget_config option

class t () =
object(self)

  val mutable _structures = None
  val mutable _video_data : Video_data.t React.event State.t option = None

  method create : item -> Widget.t Dashboard.Item.item = function
    | Chart cfg ->
       let open Widget_parameter_chart in
       let config = Option.get_exn cfg in
       let structures = self#get_structures () in
       let t =
         structures
         >|= (fun structures ->
           let video_data = self#get_video_data () in
           let chart = new t ~init:[] ~structures ~config () in
           React.E.map (fun data ->
               let data = convert_video_data config data in
               chart#append_data data) video_data
           |> React.E.keep;
           chart) in
       let w = Ui_templates.Loader.create_widget_loader t in
       Dashboard.Item.make_item
         ~name:(typ_to_string config.typ)
         w#widget

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

  method private get_structures () = match _structures with
    | Some (state : _ State.t) -> state.value
    | None ->
       let state =
         let get () =
           Requests_structure.HTTP.get ()
           |> Lwt_result.map_err Api_js.Requests.err_to_string in
         Signal.make_state ~get
           ~get_socket:Requests_structure.WS.get in
       _structures <- Some state;
       state.value

  (* XXX
   * Some thoughts about optimization:
   * Maybe this function should make request according to current widget needs
   * and when these needs change it should close an old socket and open a new one
   * with new filtering applied.
   * E.g., Requesting a socket with data for all sources is very redundant for one widget
   *)
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
