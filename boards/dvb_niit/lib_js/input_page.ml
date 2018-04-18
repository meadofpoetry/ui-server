open Lwt_result.Infix
open Containers
open Components
open Measures

let fab_class = "mdc-fixed-fab"

let make_param w =
  new Card.t ~widgets:[w] ()

let make (control:int) =
  let factory = new Widgets.Factory.t control () in
  let mer_p   = factory#create (Parameter { id = 0; typ = `Mer }) in
  let ber_p   = factory#create (Parameter { id = 0; typ = `Ber }) in
  let frq_p   = factory#create (Parameter { id = 0; typ = `Freq }) in
  let pow_p   = factory#create (Parameter { id = 0; typ = `Power }) in
  let conf t = ({ typ = t; ids=[0;1]; duration = 120000L }:Widget_chart.config) in
  let pow  = factory#create (Chart (conf `Power)) in
  let mer  = factory#create (Chart (conf `Mer)) in
  let ber  = factory#create (Chart (conf `Ber)) in
  let frq  = factory#create (Chart (conf `Freq)) in
  let br   = factory#create (Chart (conf `Bitrate)) in
  let grid = Dynamic_grid.to_grid ~vertical_compact:true ~row_height:100 ~items_margin:(10,10) ~cols:4 () in
  let items = [ Dynamic_grid.Item.to_item ~widget:mer_p ~pos:{x=0;y=0;w=1;h=1} ~value:() ()
              ; Dynamic_grid.Item.to_item ~widget:ber_p ~pos:{x=1;y=0;w=1;h=1} ~value:() ()
              ; Dynamic_grid.Item.to_item ~widget:frq_p ~pos:{x=2;y=0;w=1;h=1} ~value:() ()
              ; Dynamic_grid.Item.to_item ~widget:pow_p ~pos:{x=3;y=0;w=1;h=1} ~value:() ()
              ; Dynamic_grid.Item.to_item ~widget:pow ~pos:{x=0;y=1;w=4;h=2} ~value:() ()
              ; Dynamic_grid.Item.to_item ~widget:mer ~pos:{x=0;y=3;w=4;h=2} ~value:() ()
              ; Dynamic_grid.Item.to_item ~widget:ber ~pos:{x=0;y=5;w=4;h=2} ~value:() ()
              ; Dynamic_grid.Item.to_item ~widget:frq ~pos:{x=0;y=7;w=4;h=2} ~value:() ()
              ; Dynamic_grid.Item.to_item ~widget:br  ~pos:{x=0;y=9;w=4;h=2} ~value:() ()
              ]
  in
  let dg   = new Dynamic_grid.t ~grid ~items () in
  object
    inherit Widget.widget dg#root ()
    method on_load = dg#layout
    method on_unload = factory#destroy
  end
