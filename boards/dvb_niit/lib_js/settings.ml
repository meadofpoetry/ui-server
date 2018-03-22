open Containers
open Board_types
open Components

let (%>) = Fun.(%>)

type page_state =
  { config : WebSockets.webSocket Js.t
  ; state  : WebSockets.webSocket Js.t
  }

type signals =
  { config : config React.signal
  ; state  : Common.Topology.state React.signal
  }

module Channel = struct

  type t =
    { chan : int
    ; freq : int
    ; spec : bool
    ; name : string
    }

  let mhz = 1000000

  let to_string chan freq spec =
    if spec
    then Printf.sprintf "ТВК S%02d (%d МГц)" chan (freq / mhz)
    else Printf.sprintf "ТВК %02d (%d МГц)"  chan (freq / mhz)

  module Terrestrial = struct

    let lst : t list =
      let f = function
        | x when x <= 7 -> let chan = x + 5 in
                           let freq = (chan * 8 + 130) * mhz in
                           { chan; freq; spec = false; name = to_string chan freq false }
        | x             -> let chan = x + 13 in
                           let freq = (chan * 8 + 306) * mhz in
                           { chan; freq; spec = false; name = to_string chan freq false }
      in
      List.map f (List.range 1 56)

  end

  module Cable = struct

    let lst : t list =
      let f = function
        | 1                         -> let chan = 1 in
                                       let freq = 5250000 in
                                       let spec = false in
                                       { chan; freq; spec; name = to_string chan freq spec }
        | 2                         -> let chan = 2 in
                                       let freq = 6200000 in
                                       let spec = false in
                                       { chan; freq; spec; name = to_string chan freq spec }
        | x when x >= 3 && x <= 5   -> let chan = x in
                                       let freq = (x * 8 + 56) * mhz in
                                       let spec = false in
                                       { chan; freq; spec; name = to_string chan freq spec }
        | x when x >= 6 && x <= 13  -> let chan = x - 5 in
                                       let freq = (x * 8 + 66) * mhz in
                                       let spec = true in
                                       { chan; freq; spec; name = to_string chan freq spec }
        | x when x >= 14 && x <= 20 -> let chan = x - 8 in
                                       let freq = (x * 8 + 66) * mhz in
                                       let spec = false in
                                       { chan; freq; spec; name = to_string chan freq spec }
        | x when x >= 21 && x <= 50 -> let chan = x - 10 in
                                       let freq = (x * 8 + 66) * mhz in
                                       let spec = true in
                                       { chan; freq; spec; name = to_string chan freq spec }
        | x                         -> let chan = x - 30 in
                                       let freq = (x * 8 + 66) * mhz in
                                       let spec = false in
                                       { chan; freq; spec; name = to_string chan freq spec }
      in
      List.map f (List.range 1 99)

  end

end

class page ~s ~widgets () = object
  inherit Box.t ~gap:20 ~widgets ()

  method get_s : (settings_request,string) result React.signal = s
end

let mode ~(init : mode) =
  let mode_items = [ new Select.Base.Item.t ~text:"DVB-T2" ~value:T2 ()
                   ; new Select.Base.Item.t ~text:"DVB-T"  ~value:T  ()
                   ; new Select.Base.Item.t ~text:"DVB-C"  ~value:C  ()
                   ]
  in
  let mode       = new Select.Base.t ~label:"Стандарт" ~items:mode_items () in
  let _ = mode#select_value init in
  let _ = mode#get_menu#set_dense true in
  mode

let bw ~(init : bw) =
  let bw_items   = [ new Select.Base.Item.t ~text:"8 МГц" ~value:Bw8 ()
                   ; new Select.Base.Item.t ~text:"7 МГц" ~value:Bw7 ()
                   ; new Select.Base.Item.t ~text:"6 МГц" ~value:Bw6 ()
                   ]
  in
  let bw         = new Select.Base.t ~label:"Полоса пропускания" ~items:bw_items () in
  let _ = bw#select_value init in
  let _ = bw#get_menu#set_dense true in
  bw

let freq ~(typ  : mode)
      ~(init : int32) =
  let freq_items = List.map (fun (c:Channel.t) -> new Select.Base.Item.t ~text:c.name ~value:c.freq ())
                     (match typ with
                      | T2 | T -> Channel.Terrestrial.lst
                      | C      -> Channel.Cable.lst)
  in
  let freq  = new Select.Base.t ~label:"ТВ канал" ~items:freq_items () in
  let _ = freq#select_value (Int32.to_int init) in
  let _ = freq#get_menu#set_dense true in
  freq

let mode_box ~(typ     : mode)
      ~(s_mode  : channel_settings React.signal)
      ~(s_state : Common.Topology.state React.signal)=
  let init = React.S.value s_mode in
  let freq = freq ~typ ~init:init.freq in
  let bw   = bw ~init:init.bw in
  let plp  = new Textfield.t ~input_type:(Integer (Some (0,255))) ~box:true ~label:"PLP ID" () in
  let _    = plp#fill_in init.plp in
  let box  = new Box.t
               ~gap:20
               ~widgets:(match typ with
                         | T | C -> [ freq#widget; bw#widget ]
                         | T2    -> [ freq#widget; bw#widget; plp#widget ])
               () in
  let s    = React.S.l3 (fun freq bw plp ->
                 match freq,bw,plp with
                 | Some freq,Some bw,Some plp -> Ok { freq = Int32.of_int freq; bw; plp }
                 | _                  -> Error "mode box: some values missing")
               freq#s_selected bw#s_selected plp#s_input
  in
  let _    = React.S.map (fun (s:channel_settings) -> freq#select_value (Int32.to_int s.freq) |> ignore;
                                                      bw#select_value s.bw                    |> ignore;
                                                      plp#fill_in s.plp) s_mode
  in
  let _    = React.S.map (fun x -> let d = (match x with
                                            | `No_response | `Init -> true
                                            | `Fine                -> false)
                                   in
                                   freq#set_disabled d;
                                   bw#set_disabled d;
                                   plp#set_disabled d) s_state
  in
  box,s


let module_settings ~(id      : int)
      ~(s_mode  : config_item React.signal)
      ~(s_state : Common.Topology.state React.signal) =
  let init        = React.S.value s_mode in
  let mode        = mode ~init:init.mode in
  let t2_box,s_t2 = mode_box ~typ:T2 ~s_state ~s_mode:(React.S.map (fun x -> x.t2) s_mode) in
  let t_box,s_t   = mode_box ~typ:T  ~s_state ~s_mode:(React.S.map (fun x -> x.t)  s_mode) in
  let c_box,s_c   = mode_box ~typ:C  ~s_state ~s_mode:(React.S.map (fun x -> x.c)  s_mode) in

  let s    = React.S.l4 (fun mode t2 t c -> match mode,t2,t,c with
                                            | Some T2,Ok t2,_,_ -> Ok (id,{ mode = T2; channel = t2})
                                            | Some T,_,Ok t,_   -> Ok (id,{ mode = T ; channel = t })
                                            | Some C,_,_,Ok c   -> Ok (id,{ mode = C ; channel = c })
                                            | _                 -> Error "all: some values missing")
               mode#s_selected s_t2 s_t s_c
  in
  let box = new page ~s ~widgets:[ mode#widget; t2_box#widget; t_box#widget; c_box#widget ] () in
  let update_visibility = function
    | Some T2 -> (try Dom.removeChild box#root t_box#root with _ -> ());
                 (try Dom.removeChild box#root c_box#root with _ -> ());
                 Dom.appendChild box#root t2_box#root
    | Some T  -> (try Dom.removeChild box#root t2_box#root with _ -> ());
                 (try Dom.removeChild box#root c_box#root with _ -> ());
                 Dom.appendChild box#root t_box#root
    | Some C  -> (try Dom.removeChild box#root t2_box#root with _ -> ());
                 (try Dom.removeChild box#root t_box#root with _ -> ());
                 Dom.appendChild box#root c_box#root
    | None    -> (try Dom.removeChild box#root t2_box#root with _ -> ());
                 (try Dom.removeChild box#root t_box#root with _ -> ());
                 (try Dom.removeChild box#root c_box#root with _ -> ())
  in
  let _ = React.S.map (fun x -> update_visibility (Some x.mode)) s_mode in
  let _ = React.S.map update_visibility mode#s_selected in
  let _ = React.S.map (fun s -> mode#select_value s.mode) s_mode in
  let _ = React.S.map (function
              | `No_response | `Init -> mode#set_disabled true
              | `Fine                -> mode#set_disabled false) s_state
  in
  box

let card control ~(signals : signals) =
  let open Tabs in
  let tabs =
    List.map (fun (id,_) ->
        let page   = module_settings ~id
                       ~s_state:signals.state
                       ~s_mode:(React.S.map (fun x -> List.Assoc.get_exn ~eq:Pervasives.(=) id x) signals.config)
        in
        { href     = None
        ; content  = `Text (Printf.sprintf "Модуль %d" (succ id))
        ; disabled = false
        ; value    = page })
      (List.sort (fun (id1,_) (id2,_) -> compare id1 id2) @@ React.S.value signals.config)
  in
  let bar     = new Tabs.Tab_bar.t ~tabs () in
  let apply   = new Button.t ~label:"Применить" () in
  let actions = new Card.Actions.t ~widgets:[apply] () in
  let card    = Ui.card ~title:"Настройки" ~tab_bar:bar ~sections:[actions#widget] in
  let _       = React.E.map (fun _ -> Option.map (fun p -> match React.S.value p#get_s with
                                                           | Ok x    -> Requests.post_settings control x
                                                           | Error e -> Lwt_result.fail e)
                                        bar#get_active_value
                                      |> ignore)
                  apply#e_click
  in
  let _       = React.S.map (function
                    | `No_response | `Init -> apply#set_disabled true
                    | `Fine                -> apply#set_disabled false) signals.state
  in
  card

let layout control ~(signals : signals) =
  let card = card control ~signals in
  (* let cell = new Layout_grid.Cell.t ~widgets:[card] () in
   * let grid = new Layout_grid.t ~cells:[cell] () in
   * grid *)
  card#style##.margin   := Js.string "24px";
  card#style##.maxWidth := Js.string "640px";
  card

class settings control () = object(self)

  val mutable in_dom   = false
  val mutable observer = None
  val mutable page_state : page_state option = None

  inherit Widget.widget (Dom_html.createDiv Dom_html.document) ()

  method private observe =
    MutationObserver.observe
      ~node:Dom_html.document
      ~f:(fun _ _ ->
        let in_dom_new = (Js.Unsafe.coerce Dom_html.document)##contains self#root in
        if in_dom && (not in_dom_new)
        then Option.iter (fun (x:page_state) -> x.state##close; x.config##close; page_state <- None) page_state
        else if (not in_dom) && in_dom_new
        then (let open Lwt_result.Infix in
              Requests.get_config control
              >>= (fun config ->
                Requests.get_state control
                >>= (fun state ->
                     let state_ws,state_sock   = Requests.get_state_ws control in
                     let config_ws,config_sock = Requests.get_config_ws control in
                     let (signals:signals) = { state  = React.S.hold state  state_ws
                                             ; config = React.S.hold config config_ws } in
                     let grid = layout control ~signals in
                     Dom.appendChild self#root grid#root;
                     page_state <- Some { state  = state_sock; config = config_sock };
                     Lwt_result.return ()))
              |> ignore);
        in_dom <- in_dom_new)
      ~child_list:true
      ~subtree:true
      ()
    |> (fun o -> observer <- Some o)

  initializer
    self#observe

end
