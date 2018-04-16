open Containers
open Board_types
open Components
open Chartjs

type 'a point  = (float,'a) Line.point
type 'a points = 'a point list
type 'a data   = (int * ('a point list)) list

type _ measure =
  | Power   : float measure
  | Mer     : float measure
  | Ber     : float measure
  | Freq    : Int32.t measure
  | Bitrate : Int32.t measure

let name_of_measure : type a. a measure -> string= function
  | Power   -> "Мощность"
  | Mer     -> "MER"
  | Ber     -> "BER"
  | Freq    -> "Частота"
  | Bitrate -> "Битрейт"

let get_label id = Printf.sprintf "Модуль %d" id

let colors = Color.([ Indigo C500; Amber C500; Green C500; Cyan C500 ])

let make_chart_base ~(typ:   'a Axes.numeric)
                    ~(init:  'a data)
                    ~(event: 'a data React.event)
                    () =
  let conv = fun p -> Chartjs.Line.({ p with x = Int64.of_float (p.x *. 1000.) }) in
  let data = List.map (fun (id,data) ->
                 let data = List.map conv data in
                 Chartjs.Line.({ data = data; label = get_label id })) init in
  let config = Chartjs.Line.(new Config.t
                                 ~x_axis:(Time ("my-x-axis",Bottom,Unix,Some 120000L))
                                 ~y_axis:(Linear ("my-y-axis",Left,typ,None))
                                 ~data
                                 ())
  in
  List.iteri (fun i x -> let clr = Option.get_or ~default:(Color.Red C500) @@ List.get_at_idx i colors in
                         x#set_background_color @@ Color.rgb_of_name clr;
                         x#set_border_color     @@ Color.rgb_of_name clr;
                         x#set_cubic_interpolation_mode Chartjs.Line.Monotone;
                         x#set_fill Chartjs.Line.Disabled) config#datasets;
  config#options#x_axis#ticks#set_auto_skip_padding 2;
  let chart = new Chartjs.Line.t ~config () in
  let set   = fun ds data -> List.iter (fun point -> ds#push point) @@ List.map conv data;
                             chart#update None
  in
  let _ = React.E.map (fun datasets -> List.iter (fun (id,data) -> Option.iter (fun ds -> set ds data)
                                                                   @@ List.get_at_idx id chart#config#datasets)
                                                 datasets)
                      event in
  chart#widget

let make_chart : type a. a measure -> init:a data -> event:a data React.event -> unit -> Widget.widget = function
  | Power   -> make_chart_base ~typ:Float
  | Mer     -> make_chart_base ~typ:Float
  | Ber     -> make_chart_base ~typ:Float
  | Freq    -> make_chart_base ~typ:Int32
  | Bitrate -> make_chart_base ~typ:Int32
