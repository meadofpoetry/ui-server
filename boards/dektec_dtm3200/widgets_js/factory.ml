open Application_types
open Board_dektec_dtm3200_types
open Board_dektec_dtm3200_http_js
open Components

type item =
  | Parameter of Widget_parameter.config
  | Nw_settings
  | Recv_settings
  | Settings [@@deriving yojson]

let map_err x = Lwt_result.map_err Api_js.Http.error_to_string x

class t (control : int) () =
  let open Ui_templates.Factory in
  object(self)
    val mutable _state : Topology.state Signal.t option = None
    val mutable _status : status React.event State.t option = None
    val mutable _nw_mode : nw Signal.t option = None
    val mutable _ip_mode : ip_receive Signal.t option = None

    (** Create widget of type *)
    method create : item -> Widget.t Dashboard.Item.item = function
      | Parameter conf ->
         Widget_parameter.make self#_status conf
      | Nw_settings ->
         (fun s m -> Widget_network_settings.make ~state:s ~mode:m control)
         |> Lift.l2 self#_state self#_nw_mode
         |> Ui_templates.Loader.create_widget_loader
         |> Widget.coerce
         |> Dashboard.Item.make_item ~name:Widget_network_settings.name
              ?settings:Widget_network_settings.settings
      | Recv_settings ->
         (fun s m -> Widget_receiver_settings.make ~state:s ~mode:m control)
         |> Lift.l2 self#_state self#_ip_mode
         |> Ui_templates.Loader.create_widget_loader
         |> Widget.coerce
         |> Dashboard.Item.make_item ~name:Widget_receiver_settings.name
              ?settings:Widget_receiver_settings.settings
      | Settings ->
         (fun s nw ip -> Widget_settings.make ~state:s ~nw ~ip control)
         |> Lift.l3 self#_state self#_nw_mode self#_ip_mode
         |> Ui_templates.Loader.create_widget_loader
         |> Widget.coerce
         |> Dashboard.Item.make_item ~name:Widget_settings.name
              ?settings:Widget_settings.settings

    method destroy () =
      let iter f = function None -> () | Some x -> f x in
      iter State.finalize _status

    method available : Dashboard.available = `List [ ]

    method serialize (x : item) : Yojson.Safe.json =
      item_to_yojson x

    method deserialize (json : Yojson.Safe.json) : (item,string) result =
      item_of_yojson json

    method private _state = match _state with
      | Some state -> state.value
      | None ->
         let state =
           Signal.make_state
             ~get:(fun () -> Http_device.get_state control |> map_err)
             ~get_socket:(fun f -> Http_device.Event.get_state f control) in
         _state <- Some state;
         state.value

    method private _nw_mode = match _nw_mode with
      | Some state -> state.value
      | None ->
         let state =
           Signal.make_state
             ~get:(fun () -> Http_network.get_config control |> map_err)
             ~get_socket:(fun f -> Http_network.Event.get_config f control) in
         _nw_mode <- Some state;
         state.value

    method private _ip_mode = match _ip_mode with
      | Some state -> state.value
      | None ->
         let state =
           Signal.make_state
             ~get:(fun () -> Http_receiver.get_config control |> map_err)
             ~get_socket:(fun f -> Http_receiver.Event.get_config f control) in
         _ip_mode <- Some state;
         state.value

    method private _status = match _status with
      | Some state -> state.value
      | None ->
        let ( >>= ) = Lwt_result.( >>= ) in
        let e, set = React.E.create () in
        Lwt.async (fun () ->
            (Http_status.Event.get_status (fun _ -> function
                 | Ok x -> set x
                 | Error _ -> ()) control
             >>= fun socket ->
             let fin = fun () -> socket##close; React.E.stop ~strong:true e in
             let state = State.make ~fin e in
             _status <- Some state;
             Lwt.return_ok ()));
        e

  end
