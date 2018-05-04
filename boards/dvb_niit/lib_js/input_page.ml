open Containers
open Components

let make (control:int) =
  let factory = new Widget_factory.t control () in
  let conf t  = ({ typ = t; ids=[0;1;2;3]; duration = 120000L }:Widget_chart.config) in
  let s       = factory#create (Settings { ids = None }) in
  let ms      = factory#create (Measures { ids = None }) in
  let pow     = factory#create (Chart (conf `Power)) in
  let mer     = factory#create (Chart (conf `Mer)) in
  let ber     = factory#create (Chart (conf `Ber)) in
  let frq     = factory#create (Chart (conf `Freq)) in
  let br      = factory#create (Chart (conf `Bitrate)) in
  let (items : 'a Dashboard.Item.positioned_item list) =
    [ { item = s;      position = {x=0;y=0;w=2;h=3} }
    ; { item = ms;     position = {x=2;y=0;w=2;h=3} }
    ; { item = pow;    position = {x=0;y=3;w=4;h=2} }
    ; { item = mer;    position = {x=0;y=5;w=4;h=2} }
    ; { item = ber;    position = {x=0;y=7;w=4;h=2} }
    ; { item = frq;    position = {x=0;y=9;w=4;h=2} }
    ; { item = br;     position = {x=0;y=11;w=4;h=2} }
    ]
  in
  let dg = new Dashboard.t ~items () in
  object
    inherit Widget.widget dg#root ()
    method on_load   = dg#layout
    method on_unload = factory#destroy
  end
