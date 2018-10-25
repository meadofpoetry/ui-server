open Containers
open Components
open Board_types
open Lwt_result
open Common

type item =
  | Parameter of Widget_parameter.config
  | Nw_settings of Widget_network_settings.config option
  | Recv_settings of Widget_receiver_settings.config option
  | Settings of Widget_settings.config option [@@deriving yojson]

let map_err : 'a 'b. ('b,'a Api_js.Requests.err) Lwt_result.t -> ('b,string) Lwt_result.t =
  fun x -> Lwt_result.map_err (fun e -> Api_js.Requests.err_to_string ?to_string:None e) x

(* Widget factory *)
class t (control : int) () =
  let open Ui_templates.Factory in
  object(self)
    val mutable _state : Topology.state Signal.t option = None
    val mutable _status : status React.event State.t option = None
    val mutable _nw_mode : nw Signal.t option = None
    val mutable _ip_mode : ip Signal.t option = None

    (** Create widget of type *)
    method create : item -> Widget.t Dashboard.Item.item = function
      | Parameter conf ->
         Widget_parameter.make self#_status conf
      | Nw_settings conf   ->
         (fun s m -> Widget_network_settings.make ~state:s ~mode:m conf control)
         |> Lift.l2 self#_state self#_nw_mode
         |> Ui_templates.Loader.create_widget_loader
         |> Widget.coerce
         |> Dashboard.Item.make_item ~name:Widget_network_settings.name
              ?settings:Widget_network_settings.settings
      | Recv_settings conf ->
         (fun s m -> Widget_receiver_settings.make ~state:s ~mode:m
                       conf control)
         |> Lift.l2 self#_state self#_ip_mode
         |> Ui_templates.Loader.create_widget_loader
         |> Widget.coerce
         |> Dashboard.Item.make_item ~name:Widget_receiver_settings.name
              ?settings:Widget_receiver_settings.settings
      | Settings conf ->
         (fun s nw ip -> Widget_settings.make ~state:s ~nw ~ip conf control)
         |> Lift.l3 self#_state self#_nw_mode self#_ip_mode
         |> Ui_templates.Loader.create_widget_loader
         |> Widget.coerce
         |> Dashboard.Item.make_item ~name:Widget_settings.name
              ?settings:Widget_settings.settings

    method destroy () =
      Option.iter State.finalize _status

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
             ~get:(fun () -> Requests.Device.HTTP.get_state control |> map_err)
             ~get_socket:(fun () -> Requests.Device.WS.get_state control) in
         _state <- Some state;
         state.value

    method private _nw_mode = match _nw_mode with
      | Some state -> state.value
      | None ->
         let state =
           Signal.make_state
             ~get:(fun () -> Requests.Device.HTTP.get_mode control |> map_err)
             ~get_socket:(fun () -> Requests.Device.WS.get_mode control) in
         _nw_mode <- Some state;
         state.value

    method private _ip_mode = match _ip_mode with
      | Some state -> state.value
      | None ->
         let state =
           Signal.make_state
             ~get:(fun () -> Requests.Receiver.HTTP.get_mode control |> map_err)
             ~get_socket:(fun () -> Requests.Receiver.WS.get_mode control) in
         _ip_mode <- Some state;
         state.value

    method private _status = match _status with
      | Some state -> state.value
      | None ->
         let e, sock = Requests.Receiver.WS.get_status control in
         let fin = fun () -> sock##close; React.E.stop ~strong:true e in
         let state = State.make ~fin e in
         _status <- Some state;
         e

  end
