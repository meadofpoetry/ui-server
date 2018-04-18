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
  let pow     = factory#create (Chart (conf `Power)) in
  let mer     = factory#create (Chart (conf `Mer)) in
  let ber     = factory#create (Chart (conf `Ber)) in
  let frq     = factory#create (Chart (conf `Freq)) in
  let br      = factory#create (Chart (conf `Bitrate)) in
  let ms      = factory#create Settings in
  let items   = [ Widget_grid.Item.to_item ~pos:{x=0;y=0;w=2;h=4} ms
                ; Widget_grid.Item.to_item ~pos:{x=2;y=0;w=1;h=1} mer_p
                ; Widget_grid.Item.to_item ~pos:{x=3;y=0;w=1;h=1} ber_p
                ; Widget_grid.Item.to_item ~pos:{x=2;y=1;w=1;h=1} frq_p
                ; Widget_grid.Item.to_item ~pos:{x=3;y=1;w=1;h=1} pow_p
                ; Widget_grid.Item.to_item ~pos:{x=2;y=2;w=1;h=1} br_p
                ; Widget_grid.Item.to_item ~pos:{x=0;y=4;w=4;h=2} pow
                ; Widget_grid.Item.to_item ~pos:{x=0;y=6;w=4;h=2} mer
                ; Widget_grid.Item.to_item ~pos:{x=0;y=8;w=4;h=2} ber
                ; Widget_grid.Item.to_item ~pos:{x=0;y=10;w=4;h=2} frq
                ; Widget_grid.Item.to_item ~pos:{x=0;y=12;w=4;h=2} br
                ]
  in
  let dg      = new Widget_grid.t ~items () in
  object
    inherit Widget.widget dg#root ()
    method on_load = dg#layout
    method on_unload = factory#destroy
  end
