open Containers
open Board_types
open Components
open Chartjs

type 'a point  = (float,'a) Line.point
type 'a points = 'a point list
type 'a data   = (int * ('a point list)) list

type _ typ =
  | Power   : float typ
  | Mer     : float typ
  | Ber     : float typ
  | Freq    : Int32.t typ
  | Bitrate : Int32.t typ

type 'a config =
  { typ      : 'a typ
  ; modules  : int list
  ; duration : Int64.t
  }

type 'a chart = (int64,int64 Chartjs.Axes.Cartesian.Time.t,'a,'a Chartjs.Axes.Cartesian.Linear.t) Chartjs.Line.t

let chart_name_of_typ : type a. a typ -> string= function
  | Power   -> "Мощность"
  | Mer     -> "MER"
  | Ber     -> "BER"
  | Freq    -> "Частота"
  | Bitrate -> "Битрейт"

let get_label id = Printf.sprintf "Модуль %d" id

let colors = Color.([ Indigo C500; Amber C500; Green C500; Cyan C500 ])

let make_chart_base ~(typ:    'a Axes.numeric)
                    ~(config: 'a config)
                    ~(init:   'a data)
                    ~(event:  'a data React.event)
                    () : 'a chart =
  let conv = fun p -> Chartjs.Line.({ p with x = Int64.of_float (p.x *. 1000.) }) in
  let init = List.map (fun x -> match List.Assoc.get ~eq:Int.equal x init with
                                | Some i -> x,List.map conv i
                                | None   -> x,[]) config.modules in
  let data = List.map (fun (id,data) -> Chartjs.Line.({ data = data; label = get_label id })) init in
  let config = Chartjs.Line.(new Config.t
                                 ~x_axis:(Time ("my-x-axis",Bottom,Unix,Some config.duration))
                                 ~y_axis:(Linear ("my-y-axis",Left,typ,None))
                                 ~data
                                 ())
  in
  List.iteri (fun i x -> let clr = Option.get_or ~default:(Color.Red C500) @@ List.get_at_idx i colors in
                         x#set_background_color @@ Color.rgb_of_name clr;
                         x#set_border_color     @@ Color.rgb_of_name clr;
                         x#set_cubic_interpolation_mode Chartjs.Line.Monotone;
                         x#set_fill Chartjs.Line.Disabled)
             config#datasets;
  config#options#x_axis#ticks#set_auto_skip_padding 2;
  config#options#set_maintain_aspect_ratio false;
  let chart = new Chartjs.Line.t ~config () in
  let set   = fun ds data -> List.iter (fun point -> ds#push point) @@ List.map conv data;
                             chart#update None
  in
  let _ = React.E.map (fun datasets -> List.iter (fun (id,data) -> Option.iter (fun ds -> set ds data)
                                                                   @@ List.get_at_idx id chart#config#datasets)
                                                 datasets)
                      event in
  chart

type event = measure_response React.event
let to_event (get: Board_types.measure -> 'a option)
             (event: event) : 'a data React.event =
  React.E.map (fun (id,meas) -> [id, List.return ({ x = meas.timestamp; y = Option.get_exn (get meas) }:'a point)])
  @@ React.E.filter (fun (_,m) -> Option.is_some @@ get m) event

let to_power_event   (event:event) = to_event (fun m -> m.power)   event
let to_mer_event     (event:event) = to_event (fun m -> m.mer)     event
let to_ber_event     (event:event) = to_event (fun m -> m.ber)     event
let to_freq_event    (event:event) = to_event (fun m -> m.freq)    event
let to_bitrate_event (event:event) = to_event (fun m -> m.bitrate) event

let make_chart : type a. init:a data -> event:a data React.event -> a config -> a chart =
  fun ~init ~event config ->
  match config.typ with
  | Power   -> make_chart_base ~typ:Float ~init ~event ~config ()
  | Mer     -> make_chart_base ~typ:Float ~init ~event ~config ()
  | Ber     -> make_chart_base ~typ:Float ~init ~event ~config ()
  | Freq    -> make_chart_base ~typ:Int32 ~init ~event ~config ()
  | Bitrate -> make_chart_base ~typ:Int32 ~init ~event ~config ()
