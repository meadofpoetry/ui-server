open Board_types
open Components
open Chartjs

type page_state =
  { measures : WebSockets.webSocket Js.t
  }

let chart ~typ ~e () =
  let open Chartjs.Line in
  let data = [ { data = []; label = "Модуль 1"}
             ; { data = []; label = "Модуль 2"}
             ; { data = []; label = "Модуль 3"}
             ; { data = []; label = "Модуль 4"}
             ] in
  let config = new Config.t
                   ~x_axis:(Time ("my-x-axis",Bottom,Unix,Some 120000L))
                   ~y_axis:(Linear ("my-y-axis",Left,typ,None))
                   ~data
                   ()
  in
  let chart = new t ~config () in
  List.iter (fun x -> (match x#get_label with
                       | "Модуль 1" -> x#set_background_color @@ Color.rgb_of_name (Color.Indigo C500);
                                       x#set_border_color @@ Color.rgb_of_name (Color.Indigo C500)
                       | "Модуль 2" -> x#set_background_color @@ Color.rgb_of_name (Color.Amber C500);
                                       x#set_border_color @@ Color.rgb_of_name (Color.Amber C500)
                       | "Модуль 3" -> x#set_background_color @@ Color.rgb_of_name (Color.Green C500);
                                       x#set_border_color @@ Color.rgb_of_name (Color.Green C500)
                       | "Модуль 4" -> x#set_background_color @@ Color.rgb_of_name (Color.Cyan C500);
                                       x#set_border_color @@ Color.rgb_of_name (Color.Cyan C500));
                      x#set_cubic_interpolation_mode Monotone;
                      x#set_fill Disabled) config#datasets;
  let _ = React.E.map (fun (id,time,y) ->
              match y with
              | Some y -> let ds = CCList.get_at_idx_exn id chart#config#datasets in
                          ds#push { x = Int64.of_float (time *. 1000.); y };
                          chart#update None
              | None   -> ()) e in
  chart

let chart_card ~typ ~(f_extract:measure -> 'a option) ~title ~e_measures () =
  let e     = React.E.map (fun (id,m) -> id, m.timestamp, f_extract m) e_measures in
  let title = new Card.Title.t ~title () in
  let prim  = new Card.Primary.t ~widgets:[title] () in
  let media = new Card.Media.t ~widgets:[chart ~typ ~e ()] () in
  new Card.t ~sections:[ `Primary prim; `Media media ] ()

let chart_grid ~e_measures () =
  let pow_chart = chart_card ~title:"Мощность" ~typ:Float ~f_extract:(fun m -> m.power)   ~e_measures () in
  let mer_chart = chart_card ~title:"MER"      ~typ:Float ~f_extract:(fun m -> m.mer)     ~e_measures () in
  let ber_chart = chart_card ~title:"BER"      ~typ:Float ~f_extract:(fun m -> m.ber)     ~e_measures () in
  let frq_chart = chart_card ~title:"Частота"  ~typ:Int32 ~f_extract:(fun m -> m.freq)    ~e_measures () in
  let br_chart  = chart_card ~title:"Битрейт"  ~typ:Int32 ~f_extract:(fun m -> m.bitrate) ~e_measures () in
  let cells     = CCList.map (fun x -> let cell = new Layout_grid.Cell.t ~widgets:[x] () in
                                       cell#set_span 6;
                                       cell#set_span_phone 12;
                                       cell#set_span_tablet 12;
                                       cell)
                             [ pow_chart#widget; mer_chart#widget; ber_chart#widget;
                               frq_chart#widget; br_chart#widget ]
  in
  new Layout_grid.t ~cells ()

class measures control () = object(self)

  inherit Widget.widget (Dom_html.createDiv Dom_html.document) ()

  val mutable in_dom   = false
  val mutable observer = None
  val mutable page_state : page_state option = None

  method private observe =
    MutationObserver.observe
      ~node:Dom_html.document
      ~f:(fun _ _ ->
        let in_dom_new = (Js.Unsafe.coerce Dom_html.document)##contains self#root in
        if in_dom && (not in_dom_new)
        then CCOpt.iter (fun (x:page_state) -> x.measures##close; page_state <- None) page_state
        else if (not in_dom) && in_dom_new
        then (let e_measures,meas_sock = Requests.get_measures_ws control in
              let grid = chart_grid ~e_measures () in
              page_state <- Some { measures = meas_sock };
              Dom.appendChild self#root grid#root);
        in_dom <- in_dom_new)
      ~child_list:true
      ~subtree:true
      ()
    |> (fun o -> observer <- Some o)

  initializer
    self#observe

end
