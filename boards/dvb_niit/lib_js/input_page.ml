open Lwt_result.Infix
open Containers
open Components

let make (control:int) =
  let factory = new Widgets.Factory.t control () in
  let conf t  = ({ typ = t; ids=[0;1;2;3]; duration = 120000L }:Widget_chart.config) in
  let mer_p   = factory#create (Parameter { id = 0; typ = `Mer }) in
  let ber_p   = factory#create (Parameter { id = 0; typ = `Ber }) in
  let frq_p   = factory#create (Parameter { id = 0; typ = `Freq }) in
  let pow_p   = factory#create (Parameter { id = 0; typ = `Power }) in
  let br_p    = factory#create (Parameter { id = 0; typ = `Bitrate}) in
  let param1  = factory#create (Parameters { id = 0 }) in
  let param2  = factory#create (Parameters { id = 1 }) in
  let param3  = factory#create (Parameters { id = 2 }) in
  let param4  = factory#create (Parameters { id = 3 }) in
  let pow     = factory#create (Chart (conf `Power)) in
  let mer     = factory#create (Chart (conf `Mer)) in
  let ber     = factory#create (Chart (conf `Ber)) in
  let frq     = factory#create (Chart (conf `Freq)) in
  let br      = factory#create (Chart (conf `Bitrate)) in
  let ms      = factory#create Settings in
  let wrap x  = x in
  let items   = [ Widget_grid.Item.to_item ~pos:{x=0;y=0;w=2;h=3} @@ wrap ms
                ; Widget_grid.Item.to_item ~pos:{x=2;y=0;w=1;h=1} @@ wrap mer_p
                ; Widget_grid.Item.to_item ~pos:{x=3;y=0;w=1;h=1} @@ wrap ber_p
                ; Widget_grid.Item.to_item ~pos:{x=2;y=1;w=1;h=1} @@ wrap frq_p
                ; Widget_grid.Item.to_item ~pos:{x=3;y=1;w=1;h=1} @@ wrap pow_p
                ; Widget_grid.Item.to_item ~pos:{x=2;y=2;w=1;h=1} @@ wrap br_p
                ; Widget_grid.Item.to_item ~pos:{x=0;y=3;w=2;h=2} @@ wrap param1
                ; Widget_grid.Item.to_item ~pos:{x=2;y=3;w=2;h=2} @@ wrap param2
                ; Widget_grid.Item.to_item ~pos:{x=0;y=5;w=2;h=2} @@ wrap param3
                ; Widget_grid.Item.to_item ~pos:{x=2;y=5;w=2;h=2} @@ wrap param4
                ; Widget_grid.Item.to_item ~pos:{x=0;y=7;w=4;h=2} @@ wrap pow
                ; Widget_grid.Item.to_item ~pos:{x=0;y=9;w=4;h=2} @@ wrap mer
                ; Widget_grid.Item.to_item ~pos:{x=0;y=11;w=4;h=2} @@ wrap ber
                ; Widget_grid.Item.to_item ~pos:{x=0;y=13;w=4;h=2} @@ wrap frq
                ; Widget_grid.Item.to_item ~pos:{x=0;y=15;w=4;h=2} @@ wrap br
                ]
  in
  let dg      = new Widget_grid.t ~items () in
  object
    inherit Widget.widget dg#root ()
    method on_load = dg#layout
    method on_unload = factory#destroy
  end
