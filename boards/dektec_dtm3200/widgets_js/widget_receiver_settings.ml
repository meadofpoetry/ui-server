open Js_of_ocaml
open Application_types
open Board_dektec_dtm3200_types
open Board_dektec_dtm3200_http_js
open Components

type event =
  [ `Mode of ip_receive
  | `State of Topology.state ]

let base_class = "ip-dektec-receiver-settings"

let multicast =
  Textfield.(
    Custom
      { input_type = `Text
      ; to_string = Ipaddr.V4.to_string
      ; of_string =
          (fun x ->
            match Ipaddr.V4.of_string x with
            | Error (`Msg s) -> Error s
            | Ok x ->
                if Ipaddr.V4.is_multicast x
                then Ok x
                else Error "Not a multicast address") })

let make_checkbox ~label checked =
  let event, push = React.E.create () in
  let en = Switch.make ~checked ~on_change:(fun _ -> Lwt.return @@ push ()) () in
  let form = Form_field.make ~label ~align_end:true en in
  form, event

let make_method multicast value =
  let hide w = w#root##.style##.display := Js.string "none" in
  let show w = w#root##.style##.display := Js.string "" in
  (match value with
  | Multicast -> show multicast
  | Unicast -> hide multicast);
  let event, push = React.E.create () in
  let validation =
    Select.(
      Custom
        { to_string = string_of_int % meth_to_enum
        ; of_string =
            (fun s ->
              match int_of_string_opt s with
              | None -> Error "Bad method value"
              | Some x -> (
                match meth_of_enum x with
                | Some x -> Ok x
                | None -> Error "Bad method value")) })
  in
  let select =
    Select.make_native
      ~value
      ~label:"Метод"
      ~on_change:(fun x ->
        (match x#value with
        | None -> ()
        | Some Unicast -> hide multicast
        | Some Multicast -> show multicast);
        push ())
      ~items:(`Data [Unicast, Some "Unicast"; Multicast, Some "Multicast"])
      validation
  in
  select, event

let make_port value =
  let event, push = React.E.create () in
  let port =
    Textfield.make_textfield
      ~on_input:(fun _ _ -> Lwt.return @@ push ())
      ~label:"UDP порт"
      ~value
      (Integer (Some 0, Some 65535))
  in
  port, event

let make_multicast value =
  let event, push = React.E.create () in
  let mcast =
    Textfield.make_textfield
      ~on_input:(fun _ _ -> Lwt.return @@ push ())
      ~label:"Multicast адрес"
      ~value
      multicast
  in
  mcast, event

class t (state : Topology.state) (mode : ip_receive) (control : int) =
  let en, e_en = make_checkbox ~label:"Включить приём TSoIP" mode.enable in
  let fec, e_fec = make_checkbox ~label:"Включить FEC" mode.fec_enable in
  let mcast, e_mcast = make_multicast mode.multicast in
  let meth, e_meth = make_method mcast mode.addressing_method in
  let port, e_port = make_port mode.udp_port in
  let submit = Button.make ~label:"Применить" () in
  let buttons = Card.Actions.make_buttons [submit] in
  let actions = Card.Actions.make [buttons] in
  object (self)
    val mutable _e_change = None

    val mutable _state = state

    val mutable _on_submit = None

    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      super#append_child en;
      super#append_child fec;
      super#append_child meth;
      super#append_child mcast;
      super#append_child port;
      super#append_child actions;
      super#add_class base_class;
      super#add_class Box.CSS.root;
      super#add_class Box.CSS.vertical;
      self#notify (`State state);
      _e_change <-
        Some
          (React.E.map self#update_submit_button_state
          @@ React.E.select [e_en; e_fec; e_mcast; e_meth; e_port]);
      _on_submit <-
        Some
          (Js_of_ocaml_lwt.Lwt_js_events.clicks submit#root (fun _ _ ->
               Lwt.map ignore @@ self#submit ()))

    method! destroy () : unit = super#destroy ()

    method submit () : (unit, string) Lwt_result.t =
      match self#value with
      | None -> Lwt.return_error "Please fill the settings form"
      | Some mode ->
          let req =
            Http_receiver.(
              let ( >>= ) = Lwt_result.( >>= ) in
              set_enable mode.enable control
              >>= fun _ ->
              set_fec_enable mode.fec_enable control
              >>= fun _ ->
              set_multicast_address mode.multicast control
              >>= fun _ ->
              set_ip_to_output_delay mode.ip_to_output_delay control
              >>= fun _ ->
              set_addressing_method mode.addressing_method control
              >>= fun _ ->
              set_udp_port mode.udp_port control
              >>= fun _ ->
              set_rate_estimation_mode mode.rate_mode control
              >>= fun _ -> Lwt.return_ok ())
          in
          submit#set_loading_lwt req;
          Lwt_result.map_err Api_js.Http.error_to_string req

    method value : ip_receive option =
      let enable = (en#input)#checked in
      let fec_enable = (fec#input)#checked in
      match _state, mcast#value, port#value, meth#value with
      | `Fine, None, Some udp_port, Some Unicast ->
          Some {mode with enable; fec_enable; addressing_method = Unicast; udp_port}
      | `Fine, Some multicast, Some udp_port, Some addressing_method ->
          Some {mode with enable; fec_enable; addressing_method; multicast; udp_port}
      | _ -> None

    method set_value (x : ip_receive) : unit =
      (en#input)#toggle ~force:x.enable ();
      (fec#input)#toggle ~force:x.fec_enable ();
      meth#set_value x.addressing_method;
      mcast#set_value x.multicast;
      port#set_value x.udp_port;
      self#update_submit_button_state ()

    method notify : event -> unit =
      function
      | `Mode mode -> self#set_value mode
      | `State s ->
          _state <- s;
          let disabled =
            match s with
            | `Fine -> false
            | _ -> true
          in
          (en#input)#set_disabled disabled;
          (fec#input)#set_disabled disabled;
          meth#set_disabled disabled;
          mcast#set_disabled disabled;
          port#set_disabled disabled;
          self#update_submit_button_state ()

    method private update_submit_button_state () =
      match self#value with
      | Some _ -> submit#set_disabled false
      | None -> submit#set_disabled true
  end

let make state mode control = new t state mode control
