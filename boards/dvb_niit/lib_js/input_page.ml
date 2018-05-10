open Containers
open Components

let make (control:int) =
  let factory = new Widget_factory.t control () in
  let conf t  = (Some { typ = t; ids=[0;1;2;3]; duration = 120000L; settings = None }:Widget_chart.config option) in
  let default : Widget_factory.item Dashboard.Item.positioned_item list =
    [ { item = Settings None; position = {x=0;y=0;w=2;h=3} }
    ; { item = Measures None; position = {x=2;y=0;w=2;h=3} }
    ; { item = Chart (conf `Power);     position = {x=0;y=3;w=4;h=2} }
    ; { item = Chart (conf `Mer);       position = {x=0;y=5;w=4;h=2} }
    ; { item = Chart (conf `Ber);       position = {x=0;y=7;w=4;h=2} }
    ; { item = Chart (conf `Freq);      position = {x=0;y=9;w=4;h=2} }
    ; { item = Chart (conf `Bitrate);   position = {x=0;y=11;w=4;h=2} }
    ]
  in
  let dashboard = new Dashboard.t ~items:default factory () in
  dashboard
