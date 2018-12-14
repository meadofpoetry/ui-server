open Containers
open Components
open Common

type item =
  | Settings of Widget_settings.config option
  | T2MI_settings of Widget_t2mi_settings.config option
  | Jitter_settings of Widget_jitter_settings.config option [@@deriving yojson]

let item_to_info : item -> Dashboard.Item.info = fun item ->
  let serialized = item_to_yojson item in
  match item with
  | Settings _ ->
     Dashboard.Item.make_info ~title:Widget_settings.name
       ~thumbnail:(`Icon "settings")
       ~description:"Позволяет осуществлять настройку"
       ~serialized
       ()
  | T2MI_settings _ ->
     Dashboard.Item.make_info ~title:Widget_t2mi_settings.name
       ~thumbnail:(`Icon "settings")
       ~description:"Позволяет осуществлять настройку анализа потока T2-MI"
       ~serialized
       ()
  | Jitter_settings _ ->
     Dashboard.Item.make_info ~title:Widget_jitter_settings.name
       ~thumbnail:(`Icon "settings")
       ~description:"Позволяет осуществлять настройку измерений джиттера"
       ~serialized
       ()

let return = Lwt_result.return

let map_err : 'a 'b. ('b,'a Api_js.Requests.err) Lwt_result.t -> ('b,string) Lwt_result.t =
  fun x -> Lwt_result.map_err (fun e -> Api_js.Requests.err_to_string ?to_string:None e) x

(* Widget factory *)
class t (control : int) () =
  let open Ui_templates.Factory in
  object(self)

    val mutable _state = None
    val mutable _t2mi_mode = None
    val mutable _jitter_mode = None
    val mutable _streams : Stream.t list Signal.t option = None
    val mutable _incoming_streams : Stream.t list Signal.t option = None

    (** Create widget of type **)
    method create : item -> 'a Dashboard.Item.item = function
      | Settings conf ->
         (fun state t2mi_mode jitter_mode streams ->
           Widget_settings.make ~state ~t2mi_mode ~jitter_mode ~streams
             conf control)
         |> Lift.l4 self#state self#t2mi_mode self#jitter_mode self#incoming_streams
         |> Ui_templates.Loader.create_widget_loader
         |> Dashboard.Item.make_item ~name:Widget_settings.name
              ?settings:Widget_settings.settings
      | T2MI_settings conf ->
         (fun state mode streams ->
           Widget_t2mi_settings.make ~state ~mode ~streams conf control )
         |> Lift.l3 self#state self#t2mi_mode self#incoming_streams
         |> Ui_templates.Loader.create_widget_loader
         |> Dashboard.Item.make_item ~name:Widget_t2mi_settings.name
              ?settings:Widget_t2mi_settings.settings
      | Jitter_settings conf ->
         (fun s m -> Widget_jitter_settings.make ~state:s ~mode:m conf control)
         |> Lift.l2 self#state self#jitter_mode
         |> Ui_templates.Loader.create_widget_loader
         |> Dashboard.Item.make_item ~name:Widget_jitter_settings.name
              ?settings:Widget_jitter_settings.settings

    method destroy () : unit =
      Option.iter State.finalize _state;
      Option.iter State.finalize _t2mi_mode;
      Option.iter State.finalize _jitter_mode;

    method available : Dashboard.available =
      `List [ item_to_info (T2MI_settings None)
            ; item_to_info (Jitter_settings None)
            ; item_to_info (Settings None) ]

    method serialize (x : item) : Yojson.Safe.json =
      item_to_yojson x

    method deserialize (json : Yojson.Safe.json) : (item, string) result =
      item_of_yojson json

    (* Requests *)

    method state = match _state with
      | Some state -> state.value
      | None ->
         let state =
           Signal.make_state
             ~get:(fun () -> Requests.Device.HTTP.get_state control |> map_err)
             ~get_socket:(fun () -> Requests.Device.WS.get_state control) in
         _state <- Some state;
         state.value

    method t2mi_mode = match _t2mi_mode with
      | Some state -> state.value
      | None ->
         let state =
           Signal.make_state
             ~get:(fun () -> Requests.Device.HTTP.get_t2mi_mode control |> map_err)
             ~get_socket:(fun () -> Requests.Device.WS.get_t2mi_mode control) in
         _t2mi_mode <- Some state;
         state.value

    method jitter_mode = match _jitter_mode with
      | Some state -> state.value
      | None ->
         let state =
           Signal.make_state
             ~get:(fun () -> Requests.Device.HTTP.get_jitter_mode control |> map_err)
             ~get_socket:(fun () -> Requests.Device.WS.get_jitter_mode control) in
         _jitter_mode <- Some state;
         state.value

    method incoming_streams = match _incoming_streams with
      | Some state -> state.value
      | None ->
         let state =
           Signal.make_state
             ~get:(fun () ->
               Requests.Streams.HTTP.get_streams ~incoming:true control
               |> map_err)
             ~get_socket:(fun () ->
               Requests.Streams.WS.get_streams ~incoming:true control) in
         _incoming_streams <- Some state;
         state.value

    method streams = match _streams with
      | Some state -> state.value
      | None ->
         let state =
           Signal.make_state
             ~get:(fun () ->
               Requests.Streams.HTTP.get_streams ~incoming:false control
               |> map_err)
             ~get_socket:(fun () -> Requests.Streams.WS.get_streams control) in
         _streams <- Some state;
         state.value

  end
