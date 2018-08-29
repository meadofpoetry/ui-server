open Containers
open Components
open Board_types
open Lwt_result.Infix
open Ui_templates.Factory
open Common.Topology
open Common

type item =
  | Settings        of Widget_settings.config option
  | T2MI_settings   of Widget_t2mi_settings.config option
  | Jitter_settings of Widget_jitter_settings.config option [@@deriving yojson]

let item_to_info : item -> Dashboard.Item.info = fun item ->
  let serialized = item_to_yojson item in
  match item with
  | Settings _ ->
     Dashboard.Item.to_info ~title:Widget_settings.name
       ~thumbnail:(`Icon "settings")
       ~description:"Позволяет осуществлять настройку"
       ~serialized
       ()
  | T2MI_settings _ ->
     Dashboard.Item.to_info ~title:Widget_t2mi_settings.name
       ~thumbnail:(`Icon "settings")
       ~description:"Позволяет осуществлять настройку анализа потока T2-MI"
       ~serialized
       ()
  | Jitter_settings _ ->
     Dashboard.Item.to_info ~title:Widget_jitter_settings.name
       ~thumbnail:(`Icon "settings")
       ~description:"Позволяет осуществлять настройку измерений джиттера"
       ~serialized
       ()

let return = Lwt_result.return

let map_err : 'a 'b. ('b,'a Api_js.Requests.err) Lwt_result.t -> ('b,string) Lwt_result.t =
  fun x -> Lwt_result.map_err (fun e -> Api_js.Requests.err_to_string ?to_string:None e) x

open Factory_state

(* Widget factory *)
class t (control:int) () =
object(self)

  val _state       : state React.signal t_lwt              = empty ()
  val _t2mi_mode   : t2mi_mode option   React.signal t_lwt = empty ()
  val _jitter_mode : jitter_mode option React.signal t_lwt = empty ()
  (* val _structure   : Streams.TS.structure option React.signal t_lwt = empty () *)
  val _structs     : (Stream.ID.t * Streams.TS.structure) list React.signal t_lwt = empty ()
  val _bitrates    : (Stream.ID.t * Streams.TS.bitrate) list React.signal t_lwt = empty ()
  val _streams     : Stream.t list React.signal t_lwt = empty ()
  val _ts_errors   : Errors.t list React.event Factory_state.t = empty ()

  (** Create widget of type **)
  method create : item -> Dashboard.Item.item = function
    | Settings conf ->
       (fun state t2mi_mode jitter_mode streams->
         Widget_settings.make ~state ~t2mi_mode ~jitter_mode ~streams
           conf control)
       |> Factory_state_lwt.l4 self#state self#t2mi_mode self#jitter_mode self#streams
       |> Ui_templates.Loader.create_widget_loader
       |> Dashboard.Item.to_item ~name:Widget_settings.name
            ?settings:Widget_settings.settings
    | T2MI_settings conf ->
       (fun state mode streams ->
         Widget_t2mi_settings.make ~state ~mode ~streams conf control )
       |> Factory_state_lwt.l3 self#state self#t2mi_mode self#streams
       |> Ui_templates.Loader.create_widget_loader
       |> Dashboard.Item.to_item ~name:Widget_t2mi_settings.name
            ?settings:Widget_t2mi_settings.settings
    | Jitter_settings conf ->
       (fun s m -> Widget_jitter_settings.make ~state:s ~mode:m conf control)
       |> Factory_state_lwt.l2 self#state self#jitter_mode
       |> Ui_templates.Loader.create_widget_loader
       |> Dashboard.Item.to_item ~name:Widget_jitter_settings.name
            ?settings:Widget_jitter_settings.settings

  method destroy () : unit = Factory_state.finalize _state;
                             Factory_state.finalize _t2mi_mode;
                             Factory_state.finalize _jitter_mode;
                             Factory_state.finalize _structs;
                             Factory_state.finalize _bitrates

  method available : Dashboard.available =
    `List [ item_to_info (T2MI_settings None)
          ; item_to_info (Jitter_settings None)
          ; item_to_info (Settings None)
      ]

  method serialize (x : item) : Yojson.Safe.json = item_to_yojson x
  method deserialize (json : Yojson.Safe.json) : (item,string) result = item_of_yojson json

  (* Requests *)

  method state =
    Factory_state_lwt.get_value_as_signal
      ~get:(fun ()        -> Requests.Device.HTTP.get_state control |> map_err)
      ~get_socket:(fun () -> Requests.Device.WS.get_state control)
      _state

  method t2mi_mode =
    Factory_state_lwt.get_value_as_signal
      ~get:(fun ()        -> Requests.Device.HTTP.get_t2mi_mode control |> map_err)
      ~get_socket:(fun () -> Requests.Device.WS.get_t2mi_mode control)
      _t2mi_mode

  method jitter_mode =
    Factory_state_lwt.get_value_as_signal
      ~get:(fun () ->        Requests.Device.HTTP.get_jitter_mode control |> map_err)
      ~get_socket:(fun () -> Requests.Device.WS.get_jitter_mode control)
      _jitter_mode

  (* method structure id =
   *   Factory_state_lwt.get_value_as_signal
   *     ~get:(fun () -> Requests.Streams.HTTP.get_structure ~limit:1 ~id control
   *                     >>= (function
   *                          | Raw x ->
   *                             Lwt_result.return (match x.data with
   *                                                | [ (_, x) ] -> Some x
   *                                                | _   -> None)
   *                          | _ -> Lwt.fail_with "compressed")
   *                     |> map_err)
   *     ~get_socket:(fun () ->
   *       let ev, sock = Requests.Streams.WS.get_structure ~id control in
   *       React.E.map Option.return ev, sock)
   *     _structure *)

  method stream id =
    self#streams
    >|= fun streams ->
    React.S.map (fun streams ->
        List.find_opt (fun (stream:Stream.t) ->
            Stream.ID.equal stream.id id) streams)
      streams

  method streams =
    Factory_state_lwt.get_value_as_signal
      ~get:(fun () ->
        Requests.Streams.HTTP.get_streams ~compress:true ~limit:1 control
        >>= (function
             | Compressed x ->
                List.filter_map (function
                    | s, `Now -> Some s
                    | _       -> None) x.data
                |> Lwt_result.return
             | _ -> Lwt.fail_with "raw")
        |> map_err)
      ~get_socket:(fun () -> Requests.Streams.WS.get_streams control)
      _streams

  (* method ts_errors =
   *   match _ts_errors.value with
   *   | Some x -> Factory_state.succ_ref _ts_errors; x
   *   | None   ->
   *      Factory_state.set_ref _ts_errors 1;
   *      let e,sock = Requests.Errors.WS.TS.get_errors control in
   *      _ts_errors.value <- Some e;
   *      _ts_errors.fin   <- (fun () -> sock##close; React.E.stop ~strong:true e);
   *      e *)

end
