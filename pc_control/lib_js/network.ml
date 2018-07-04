open Components
open Common.User
open Api_js.Requests.Json_request

open Lwt.Infix

module Requests = struct

  open Common.Uri

  (* TODO fix relative addr *)
  let get_config () =
    let from_err = function `String s -> Ok s | _ -> Error "" in
    get_result ?scheme:None ?host:None ?port:None
               ~from:Network_config.of_yojson
               ~from_err
               ~path:Path.Format.("api/network/config" @/ empty)
               ~query:Query.empty

  let post_config conf =
    post_result_unit ?scheme:None ?host:None ?port:None ?from_err:None
                     ~contents:(Network_config.to_yojson conf)
                     ~path:Path.Format.("api/network/config" @/ empty)
                     ~query:Query.empty

end

let make_eth (eth : Network_config.ethernet_conf) =
  let eth_head  = new Card.Primary.t ~widgets:[new Card.Primary.title "Настройки устройства" ()] () in

  let of_string x = match Macaddr.of_string x with
    | Some v -> Ok v
    | None   -> Error "Неверный мак адрес"
  in
  let to_string x = Macaddr.to_string x in
  let address    = new Textfield.t
                       ~input_id:"mac-addr"
                       ~label:"MAC адрес"
                       ~input_type:(Widget.Custom (of_string, to_string))
                       ()
  in

  let signal, push = React.S.create eth in
  
  let set (eth : Network_config.ethernet_conf) = address#fill_in eth.mac_address in

  let media      = new Card.Media.t ~widgets:[new Box.t ~vertical:true ~widgets:[ address#widget ] ()] () in
  media#style##.margin := Js.string "15px";
  let eth_sets  = new Card.t ~widgets:[ eth_head#widget; media#widget ] () in
  
  let signal = Lwt_react.S.l2
                 (fun (config : Network_config.ethernet_conf) mac_address ->
                   match mac_address with
                   | None -> config
                   | Some mac_address -> { config with mac_address })
                 signal address#s_input
  in
  
  eth_sets, signal, set
                
let make_dns (dns : Network_config.v4 list) =
  let make_dns_entry del_dns addr =
    let text        = Ipaddr.V4.to_string addr in
    let del_button  = new Button.t ~label:"delete" () in
    let item        = new Item_list.Item.t ~text ~meta:del_button () in
    Lwt_react.E.map (fun _ -> del_dns item addr) del_button#e_click |> ignore;
    item
  in

  let header = new Typography.Text.t ~font:Typography.Subheading_1 ~text:"Список DNS" () in
  let list   = new Item_list.t ~items:[] () in

  let address  = new Textfield.t ~input_id:"address-dns" ~label:"Адрес" ~input_type:Widget.IPV4 () in
  let add_but  = new Button.t ~label:"Добавить" () in
  let add_box  = new Box.t ~vertical:false ~widgets:[address#widget; add_but#widget] () in

  let full_box = new Box.t ~widgets:[header#widget; list#widget; add_box#widget] () in

  let signal, push = React.S.create [] in

  let del_dns item addr =
    list#remove_item item;
    push
    @@ List.filter (fun dns -> not (Network_config.equal_v4 addr dns))
    @@ React.S.value signal
  in
  let add_dns addr =
    let rlst = React.S.value signal in
    if List.exists (Network_config.equal_v4 addr) rlst
    then failwith "dns exists"; (* TODO fix *)
    let entry = make_dns_entry del_dns addr in
    list#add_item entry;
    push (addr::rlst)
  in
  let set dns =
    list#set_empty ();
    push [];
    List.iter add_dns dns
  in
  let set_disabled flag =
    address#set_disabled flag;
    add_but#set_disabled flag;
  in

  Lwt_react.E.keep @@
    Lwt_react.E.map (fun _ ->
        match React.S.value address#s_input with
        | Some addr -> add_dns addr
        | _ -> ())
      add_but#e_click;
  
  full_box, signal, set, set_disabled
                  
let make_routes (routes : Network_config.address list) =
  let make_route_entry del_route route =
    let (addr,mask) = route in
    let text        = (Ipaddr.V4.to_string addr) ^ "/" ^ (Int32.to_string mask) in
    let del_button  = new Button.t ~label:"delete" () in
    let item        = new Item_list.Item.t ~text ~meta:del_button () in
    Lwt_react.E.map (fun _ -> del_route item route) del_button#e_click |> ignore;
    item
  in

  let header = new Typography.Text.t ~font:Typography.Subheading_1 ~text:"Список статических маршрутов" () in
  let list   = new Item_list.t ~items:[] () in
  
  let address  = new Textfield.t ~input_id:"address-route" ~label:"Адрес" ~input_type:Widget.IPV4 () in
  let mask     = new Textfield.t ~input_id:"mask-route" ~label:"Маска подсети" ~input_type:(Widget.Integer (Some 0, Some 32)) () in
  let add_but  = new Button.t ~label:"Добавить" () in
  let add_box  = new Box.t ~vertical:false ~widgets:[address#widget; mask#widget; add_but#widget] () in

  let full_box = new Box.t ~widgets:[header#widget; list#widget; add_box#widget] () in

  let signal, push = React.S.create [] in

  let del_route item (addr, mask) =
    list#remove_item item;
    push
    @@ List.filter (fun route -> not (Network_config.equal_address (addr,mask) route))
    @@ React.S.value signal
  in
  let add_route (addr, mask) =
    let rlst = React.S.value signal in
    if List.exists (Network_config.equal_address (addr,mask)) rlst
    then failwith "route exists"; (* TODO fix *)
    let entry = make_route_entry del_route (addr, mask) in
    list#add_item entry;
    push ((addr, mask)::rlst)
  in
  let set routes =
    list#set_empty ();
    push [];
    List.iter add_route routes
  in
  let set_disabled flag =
    address#set_disabled flag;
    mask#set_disabled flag;
    add_but#set_disabled flag;
  in

  Lwt_react.E.keep @@
    Lwt_react.E.map (fun _ ->
        match React.S.value address#s_input, React.S.value mask#s_input with
        | Some addr, Some mask -> add_route (addr, Int32.of_int mask)
        | _ -> ())
      add_but#e_click;
  
  full_box, signal, set, set_disabled

let make_ipv4 (ipv4 : Network_config.ipv4_conf) =
  let ipv4_head  = new Card.Primary.t ~widgets:[new Card.Primary.title "Настройки IP" ()] () in

  let meth = new Form_field.t
               ~input:(new Switch.t ~input_id:"autoconf" ())
               ~label:"Автоматическая настройка" ()
  in
  
  let address    = new Textfield.t ~input_id:"address-ipv4" ~label:"Адрес" ~input_type:Widget.IPV4 () in
  let mask       = new Textfield.t ~input_id:"mask-ipv4"  ~label:"Маска подсети" ~input_type:(Widget.Integer (Some 0, Some 32)) () in
  let gateway    = new Textfield.t ~input_id:"gateway" ~label:"Шлюз" ~input_type:Widget.IPV4 () in
  let dns, dns_s, dns_set, dns_disable = make_dns ipv4.dns in
  let routes, routes_s, routes_set, routes_disable = make_routes ipv4.routes.static in

  let signal, push = React.S.create ipv4 in
  
  let set (ipv4 : Network_config.ipv4_conf) =
    meth#input_widget#set_checked (ipv4.meth = Auto);
    address#fill_in @@ fst ipv4.address;
    mask#fill_in (Int32.to_int @@ snd ipv4.address);
    CCOpt.iter gateway#fill_in ipv4.routes.gateway;
    dns_set ipv4.dns;
    routes_set ipv4.routes.static;
    push ipv4
  in

  (* disable settings on Auto config *)
  Lwt_react.S.keep @@
    Lwt_react.S.map (fun disabled -> address#set_disabled disabled;
                                     mask#set_disabled disabled;
                                     gateway#set_disabled disabled;
                                     dns_disable disabled;
                                     routes_disable disabled) meth#input_widget#s_state;

  (* disable routes on gateway config *)
  Lwt_react.S.keep @@
    Lwt_react.S.map (function None -> routes_disable false | _ -> routes_disable true) gateway#s_input;

  let media      = new Card.Media.t ~widgets:[new Box.t ~vertical:true ~widgets:[ meth#widget;
                                                                                  address#widget;
                                                                                  mask#widget;
                                                                                  gateway#widget;
                                                                                  dns#widget;
                                                                                  routes#widget ] ()
                     ] ()
  in
  media#style##.margin := Js.string "15px";
  let ipv4_sets  = new Card.t
                     ~widgets:[ ipv4_head#widget
                              ; media#widget
                     ] ()
  in
  
  let signal = Lwt_react.S.l6 (fun (config : Network_config.ipv4_conf) meth address mask gateway routes ->
                   { config with
                     meth    = if meth then Auto else Manual
                   ; address =
                       (CCOpt.get_or address ~default:(fst config.address), CCOpt.get_or mask ~default:(snd config.address))
                   ; routes = { gateway = gateway; static = routes } }) (* TODO fix *)
                 signal
                 meth#input_widget#s_state
                 address#s_input
                 (Lwt_react.S.map (CCOpt.map Int32.of_int) mask#s_input)
                 gateway#s_input
                 routes_s
  in
  let signal = Lwt_react.S.l2 (fun (config : Network_config.ipv4_conf) dns -> { config with dns } ) signal dns_s in
  
  ipv4_sets, signal, set
                
let make_card is_root post (config : Network_config.t) =
  let warning    = new Dialog.t
                     ~title:"Внимание!"
                     ~actions:[ new Dialog.Action.t ~typ:`Decline ~label:"Отмена" ()
                              ; new Dialog.Action.t ~typ:`Accept ~label:"Применить" () ]
                     ~content:(`String "Применение настроек может привести к разрыву соединения. Вы уверены, что хотите применить данные настройки?") ()
  in
  Dom.appendChild Dom_html.document##.body warning#root;
  
  let eth_sets, eth_s, eth_set = make_eth config.ethernet in
  let ipv4_sets, ipv4_s, ipv4_set  = make_ipv4 config.ipv4 in

  let apply      = new Button.t ~label:"Применить" () in
  apply#set_disabled (not is_root);

  let signal, push = React.S.create config in

  let set (config : Network_config.t) =
    eth_set config.ethernet;
    ipv4_set config.ipv4;
    push config
  in

  (* init *)
  set config;
  let signal = Lwt_react.S.l3
                 (fun (config : Network_config.t) ipv4 ethernet -> { config with ipv4; ethernet })
                 signal ipv4_s eth_s
  in
  
  Lwt_react.E.keep @@
    Lwt_react.E.map (fun _ -> warning#show_await ()
                              >>= function
                              | `Accept -> post @@ Lwt_react.S.value signal
                              | `Cancel -> Lwt.return_unit)
      apply#e_click;

  let box = new Box.t ~vertical:true ~widgets:[eth_sets#widget; ipv4_sets#widget; apply#widget] () in
  List.iter (fun card -> card#style##.marginBottom := Js.string "15px") box#widgets;
  box, set

let page user =
  let is_root = user = `Root in
  Requests.get_config () >>= function
  | Error (`Data (_,e)) -> Lwt.fail_with e
  | Error _ -> Lwt.fail_with "unknown error"
  | Ok config ->
    let event, push = Lwt_react.E.create () in
    let post new_config =
      Requests.post_config new_config
      >>= function
      | Ok ()   -> (push new_config; Lwt.return_unit)
      | Error _ -> Requests.get_config () >>= function
                   | Error _ -> (push config; Lwt.return_unit)
                   | Ok config -> (push config; Lwt.return_unit)
    in
    let card, set = make_card is_root post config in
    Lwt_react.E.keep @@ Lwt_react.E.map (fun config -> print_endline (Yojson.Safe.pretty_to_string @@ Network_config.to_yojson config); set config) event;
    Lwt.return card
