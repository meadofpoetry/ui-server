open Js_of_ocaml
open Application_types
open Board_dektec_dtm3200_types
open Board_dektec_dtm3200_http_js
open Components

let base_class = "ip-dektec-receiver-settings"

let multicast = Textfield.(
    Custom { input_type = `Text
           ; to_string = Ipaddr.V4.to_string
           ; of_string = fun x ->
               match Ipaddr.V4.of_string x with
               | Error `Msg s -> Error s
               | Ok x ->
                 if Ipaddr.V4.is_multicast x
                 then Ok x
                 else Error "Not a multicast address"
           })

let make_checkbox ~label () =
  let signal, push = React.S.create false in
  let en = Switch.make ~on_change:(fun x -> Lwt.return @@ push x#checked) () in
  let form =
    Form_field.make ~label
      ~align_end:true
      en in
  form, signal

let make_port () =
  let port =
    Textfield.make_textfield
      ~label:"UDP порт"
      (Integer (Some 0, Some 65535)) in
  port

let make_multicast () =
  let mcast =
    Textfield.make_textfield
      ~input_id:"mcast"
      ~label:"Multicast адрес"
      multicast in
  mcast

let name = "Настройки. Приём"
let settings = None

let ( >>= ) = Lwt.( >>= )

type event =
  [ `Ip_receive_mode of ip_receive
  | `State of Topology.state
  ]

class t (state : Topology.state) (mode : ip_receive) (control : int) =
  let en, s_en = make_checkbox ~label:"Включить приём TSoIP" () in
  let fec, s_fec = make_checkbox ~label:"Включить FEC" () in
  let meth, s_meth = make_checkbox ~label:"Включить приём Multicast" () in
  let port = make_port () in
  let mcast = make_multicast () in
  let submit = Button.make ~label:"Применить" () in
  let buttons = Card.Actions.make_buttons [submit] in
  let actions = Card.Actions.make [buttons] in
  let () = ignore (state, s_en, s_fec, s_meth) in
  object(self)
    val mutable _on_submit = None
    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      super#append_child en;
      super#append_child fec;
      super#append_child meth;
      super#append_child port;
      super#append_child mcast;
      super#append_child actions;
      super#add_class base_class;
      self#set_value mode;
      _on_submit <- Some (Events.clicks submit#root (fun _ _ ->
          match self#value with
          | None -> Lwt.return ()
          | Some x ->
            self#request_ x
            >>= fun _ -> Lwt.return ()))

    method! destroy () : unit =
      super#destroy ()

    method notify : event -> unit = function
      | `Ip_receive_mode mode -> self#set_value mode
      | `State s ->
        let disabled = match s with `Fine -> false | _ -> true in
        en#input#set_disabled disabled;
        fec#input#set_disabled disabled;
        meth#input#set_disabled disabled;
        mcast#set_disabled disabled;
        port#set_disabled disabled

    method value : ip_receive option =
      match mcast#value, port#value with
      | Some multicast, Some udp_port ->
        Some { mode with
               enable = en#input#checked
             ; fec_enable = fec#input#checked
             ; addressing_method = (if meth#input#checked then Multicast else Unicast)
             ; multicast = multicast
             ; udp_port
             }
      | _ -> None

    method set_value (x : ip_receive) : unit =
      en#input#toggle ~force:x.enable ();
      fec#input#toggle ~force:x.fec_enable ();
      meth#input#toggle ~force:(match x.addressing_method with
          | Unicast -> false
          | Multicast -> true) ();
      mcast#set_value x.multicast;
      port#set_value x.udp_port

    method private request_ (x : ip_receive) =
      let req = Http_receiver.(
          let ( >>= ) = Lwt_result.( >>= ) in
          set_enable x.enable control
          >>= fun _ -> set_fec_enable x.fec_enable control
          >>= fun _ -> set_multicast_address x.multicast control
          >>= fun _ -> set_ip_to_output_delay x.ip_to_output_delay control
          >>= fun _ -> set_addressing_method x.addressing_method control
          >>= fun _ -> set_udp_port x.udp_port control
          >>= fun _ -> set_rate_estimation_mode x.rate_mode control
          >>= fun _ -> Lwt.return_ok ()) in
      submit#set_loading_lwt req;
      req
  end

let make state mode control =
  new t state mode control
