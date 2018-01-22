open Board_types
open Components

let (%>) = CCFun.(%>)

type channel =
  { chan : int
  ; freq : int
  ; spec : bool
  ; name : string
  }

class page ~s ~widgets () = object
  inherit Box.t ~gap:20 ~widgets ()

  method get_s : (settings_request,string) result React.signal = s
end

let mhz = 1000000

let channel_to_string chan freq spec =
  if spec
  then Printf.sprintf "ТВК S%02d (%d МГц)" chan (freq / mhz)
  else Printf.sprintf "ТВК %02d (%d МГц)"  chan (freq / mhz)

let t_freq =
  let f = function
    | x when x <= 7 -> let chan = x + 5 in
                       let freq = (chan * 8 + 130) * mhz in
                       { chan; freq; spec = false; name = channel_to_string chan freq false }
    | x             -> let chan = x + 13 in
                       let freq = (chan * 8 + 306) * mhz in
                       { chan; freq; spec = false; name = channel_to_string chan freq false }
  in
  CCList.map f (CCList.range 1 56)

let c_freq =
  let f = function
    | 1                         -> let chan = 1 in
                                   let freq = 5250000 in
                                   let spec = false in
                                   { chan; freq; spec; name = channel_to_string chan freq spec }
    | 2                         -> let chan = 2 in
                                   let freq = 6200000 in
                                   let spec = false in
                                   { chan; freq; spec; name = channel_to_string chan freq spec }
    | x when x >= 3 && x <= 5   -> let chan = x in
                                   let freq = (x * 8 + 56) * mhz in
                                   let spec = false in
                                   { chan; freq; spec; name = channel_to_string chan freq spec }
    | x when x >= 6 && x <= 13  -> let chan = x - 5 in
                                   let freq = (x * 8 + 66) * mhz in
                                   let spec = true in
                                   { chan; freq; spec; name = channel_to_string chan freq spec }
    | x when x >= 14 && x <= 20 -> let chan = x - 8 in
                                   let freq = (x * 8 + 66) * mhz in
                                   let spec = false in
                                   { chan; freq; spec; name = channel_to_string chan freq spec }
    | x when x >= 21 && x <= 50 -> let chan = x - 10 in
                                   let freq = (x * 8 + 66) * mhz in
                                   let spec = true in
                                   { chan; freq; spec; name = channel_to_string chan freq spec }
    | x                         -> let chan = x - 30 in
                                   let freq = (x * 8 + 66) * mhz in
                                   let spec = false in
                                   { chan; freq; spec; name = channel_to_string chan freq spec }
  in
  CCList.map f (CCList.range 1 99)

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
  let freq_items = CCList.map (fun c -> new Select.Base.Item.t ~text:c.name ~value:c.freq ())
                              (match typ with
                               | T2 | T -> t_freq
                               | C      -> c_freq)
  in
  let freq       = new Select.Base.t ~label:"ТВ канал" ~items:freq_items () in
  let _ = freq#select_value (Int32.to_int init) in
  let _ = freq#get_menu#set_dense true in
  freq

let mode_box ~(typ   : mode)
             ~(init  : channel_settings)
             ~(event : channel_settings React.event) =
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
  let _    = React.E.map (fun (s:channel_settings) -> freq#select_value (Int32.to_int s.freq) |> ignore;
                                                      bw#select_value s.bw                    |> ignore;
                                                      plp#fill_in s.plp) event
  in
  box,s


let module_settings ~(id    : int)
                    ~(init  : config_item)
                    ~(event : config_item React.event) =
  let mode        = mode ~init:init.mode in

  let t2_box,s_t2 = mode_box ~typ:T2 ~init:init.t2 ~event:(React.E.map (fun x -> x.t2) event) in
  let t_box,s_t   = mode_box ~typ:T  ~init:init.t  ~event:(React.E.map (fun x -> x.t)  event) in
  let c_box,s_c   = mode_box ~typ:C  ~init:init.c  ~event:(React.E.map (fun x -> x.c)  event) in

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
  let _ = React.E.map (fun x -> update_visibility (Some x.mode)) event in
  let _ = React.S.map update_visibility mode#s_selected in
  let _ = React.E.map (fun s -> mode#select_value s.mode) event in
  box

let card control
         ~(init  : config)
         ~(event : config React.event) =
  let open Tabs in
  let tabs =
    CCList.map (fun (id,s) ->
        let page   = module_settings ~id
                                     ~init:s
                                     ~event:(React.E.map (fun x -> print_endline "config changed!";
                                                                   CCList.Assoc.get_exn id x) event)
        in
        { href     = None
        ; content  = `Text (Printf.sprintf "Модуль %d" (succ id))
        ; disabled = false
        ; value    = page })
               (CCList.sort (fun (id1,_) (id2,_) -> compare id1 id2) init)
  in
  let bar     = new Tabs.Tab_bar.t ~tabs () in
  let apply   = new Button.t ~label:"Применить" () in
  let actions = new Card.Actions.t ~widgets:[apply] () in
  let card    = Ui.card ~title:"Настройки" ~tab_bar:bar ~sections:[`Actions actions] in
  let _       = React.E.map (fun _ -> CCOpt.map (fun p -> match React.S.value p#get_s with
                                                          | Ok x    -> Requests.post_settings control x
                                                          | Error e -> Lwt_result.fail e)
                                                bar#get_active_value
                                      |> ignore)
                            apply#e_click
  in
  card

let layout control
           ~(init  : config)
           ~(event : config React.event) =
  let card = card control ~init ~event in
  let cell = new Layout_grid.Cell.t ~widgets:[card] () in
  let grid = new Layout_grid.t ~cells:[cell] () in
  grid

class t control () = object(self)

  val mutable in_dom = false
  val mutable state  = None
  val mutable observer = None

  inherit Widget.widget (Dom_html.createDiv Dom_html.document) ()

  method private observe =
    MutationObserver.observe
      ~node:Dom_html.document
      ~f:(fun _ _ -> let in_dom_new = (Js.Unsafe.coerce Dom_html.document)##contains self#root in
                     print_endline ((string_of_bool in_dom) ^ " " ^ (string_of_bool in_dom_new));
                     (match in_dom,in_dom_new with
                      | true,false -> Printf.printf "deleted dvb settings";
                                      CCOpt.iter (fun x -> x##close; state <- None) state
                      | _          -> ());
                     in_dom <- in_dom_new)
      ~child_list:true
      ~subtree:true
      ()
    |> (fun o -> observer <- Some o)

  initializer
    self#observe;
    (let open Lwt_result.Infix in
     Requests.get_config control
     >>= (fun init ->
       let event,sock = Requests.get_config_ws control in
       let grid = layout control ~init ~event in
       Dom.appendChild self#root grid#root;
       state <- Some sock;
       Lwt_result.return ())
     |> ignore)

end

let page control =
  (new t control ())#root, (fun () -> ())
