open Containers
open Components
open Board_types
open Lwt_result
open Ui_templates.Factory
open Common

type item =
  | Parameter     of Widget_parameter.config
  | Nw_settings   of Widget_network_settings.config option
  | Recv_settings of Widget_receiver_settings.config option
  | Settings      of Widget_settings.config option [@@deriving yojson]

open Factory_state

let map_err : 'a 'b. ('b,'a Api_js.Requests.err) Lwt_result.t -> ('b,string) Lwt_result.t =
  fun x -> Lwt_result.map_err (fun e -> Api_js.Requests.err_to_string ?to_string:None e) x

(* Widget factory *)
class t (control:int) () =
object(self)
  val mutable _state   : Topology.state React.signal t_lwt  = empty ()
  val mutable _status  : status React.event Factory_state.t = empty ()
  val mutable _nw_mode : nw React.signal t_lwt              = empty ()
  val mutable _ip_mode : ip React.signal t_lwt              = empty ()

  (** Create widget of type **)
  method create : item   -> Dashboard.Item.item = function
    | Parameter conf     ->
       Widget_parameter.make self#_status conf
    | Nw_settings conf   ->
       (fun s m -> Widget_network_settings.make ~state:s ~mode:m conf control)
       |> Factory_state_lwt.l2 self#_state self#_nw_mode
       |> Ui_templates.Loader.create_widget_loader
       |> Dashboard.Item.to_item ~name:Widget_network_settings.name
                                 ?settings:Widget_network_settings.settings
    | Recv_settings conf ->
       (fun s m -> Widget_receiver_settings.make ~state:s ~mode:m
                                                 conf control)
       |> Factory_state_lwt.l2 self#_state self#_ip_mode
       |> Ui_templates.Loader.create_widget_loader
       |> Dashboard.Item.to_item ~name:Widget_receiver_settings.name
                                 ?settings:Widget_receiver_settings.settings
    | Settings conf ->
       (fun s nw ip -> Widget_settings.make ~state:s ~nw ~ip conf control)
       |> Factory_state_lwt.l3 self#_state self#_nw_mode self#_ip_mode
       |> Ui_templates.Loader.create_widget_loader
       |> Dashboard.Item.to_item ~name:Widget_settings.name
                                 ?settings:Widget_settings.settings

  method destroy () = finalize _status

  method available : Dashboard.available = `List [ ]
  method serialize (x:item) : Yojson.Safe.json = item_to_yojson x
  method deserialize (json:Yojson.Safe.json) : (item,string) result = item_of_yojson json

  method private _state =
    Factory_state_lwt.get_value_as_signal
      ~get:(fun () -> Requests.Device.HTTP.get_state control |> map_err)
      ~get_socket:(fun () -> Requests.Device.WS.get_state control)
      _state

  method private _nw_mode =
    Factory_state_lwt.get_value_as_signal
      ~get:(fun () -> Requests.Device.HTTP.get_mode control |> map_err)
      ~get_socket:(fun () -> Requests.Device.WS.get_mode control)
      _nw_mode

  method private _ip_mode =
    Factory_state_lwt.get_value_as_signal
      ~get:(fun () -> Requests.Receiver.HTTP.get_mode control |> map_err)
      ~get_socket:(fun () -> Requests.Receiver.WS.get_mode control)
      _ip_mode

  method private _status = match _status.value with
    | Some x -> succ_ref _status; x
    | None   -> set_ref _status 1;
                let e,sock = Requests.Receiver.WS.get_status control in
                _status.value <- Some e;
                _status.fin <- (fun () -> sock##close; React.E.stop ~strong:true e);
                e

end
