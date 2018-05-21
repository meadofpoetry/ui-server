open Components
open Common.User
open Api_js.Requests.Json_request

open Lwt.Infix
   
module Requests = struct

  (* TODO fix relative addr *)
  let get_config () =
    get_result Network_config.of_yojson ~from_err:(function `String s -> Ok s | _ -> Error "") "../api/network/config"

  let post_config conf =
    post_result ~contents:(Network_config.to_yojson conf) (fun _ -> Ok ()) "../api/network/config"
    
end

let make_ipv4 (config : Network_config.ipv4_conf React.signal) =
  let ipv4_head  = new Card.Primary.t ~widgets:[new Card.Primary.title "Настройки IP" ()] () in
  let address    = new Textfield.t ~input_id:"Адрес" ~input_type:Widget.IPV4 () in
  let mask       = new Textfield.t ~input_id:"Маска подсети" ~input_type:(Widget.Integer (Some 0, Some 32)) () in
  let gateway    = new Textfield.t ~input_id:"Шлюз" ~input_type:Widget.IPV4 () in
  let ipv4_sets  = new Card.t
                     ~widgets:[ ipv4_head#widget
                              ; (new Card.Media.t ~widgets:[address#widget; mask#widget; gateway#widget] ())#widget
                     ] ()
  in
  Lwt_react.S.keep @@
    Lwt_react.S.map (fun (x : Network_config.ipv4_conf) -> address#fill_in x.address) config;
  Lwt_react.S.keep @@
    Lwt_react.S.map (fun (x : Network_config.ipv4_conf) -> mask#fill_in (Int32.to_int x.mask)) config;
  Lwt_react.S.keep @@
    Lwt_react.S.map (fun (x : Network_config.ipv4_conf) -> gateway#fill_in x.gateway) config;

  let signal = Lwt_react.S.l4 (fun (config : Network_config.ipv4_conf) address mask gateway ->
                   { config with
                     address = CCOpt.get_or address ~default:config.address
                   ; mask    = CCOpt.get_or mask ~default:config.mask
                   ; gateway = CCOpt.get_or gateway ~default:config.gateway})
                 config address#s_input (Lwt_react.S.map (CCOpt.map Int32.of_int) mask#s_input) gateway#s_input
  in
  signal, ipv4_sets
                
let make_card is_root post (config : Network_config.t React.signal) =
  (*let mac_field  = 
  let hard_sets  = *)
  let ipv4_s, ipv4_sets  = make_ipv4 @@ Lwt_react.S.map (fun (c : Network_config.t) -> c.ipv4) config in

  let apply      = new Button.t ~label:"Применить" () in
  apply#set_disabled (not is_root);
  
  let signal = Lwt_react.S.l2 (fun (config : Network_config.t) ipv4 -> { config with ipv4 }) config ipv4_s in
  
  Lwt_react.E.keep @@
    Lwt_react.E.map (fun _ -> post @@ Lwt_react.S.value signal) apply#e_click;

  new Box.t ~vertical:true ~widgets:[ipv4_sets#widget; apply#widget] ()

let page user =
  let is_root = user = `Root in
  Requests.get_config () >>= function
  | Error (`Data (_,e)) -> Lwt.fail_with e
  | Error _ -> Lwt.fail_with "unknown error"
  | Ok config ->
    let signal, push = Lwt_react.S.create config in
    let post new_config =
      Requests.post_config config
      >>= function
      | Ok ()   -> (push new_config; Lwt.return_unit)
      | Error _ -> (push (Lwt_react.S.value signal);
                    print_endline "post_config: error";
                    Lwt.return_unit)
    in
    Lwt.return @@ make_card is_root post signal
