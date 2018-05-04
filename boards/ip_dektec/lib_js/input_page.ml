open Containers
open Components

let make (control:int) =
  let factory = new Widget_factory.t control () in
  let in_br   = factory#create (Parameter { typ = `Input_bitrate }) in
  let jt      = factory#create (Parameter { typ = `Jitter_tolerance }) in
  let tpperip = factory#create (Parameter { typ = `TP_per_IP }) in
  let status  = factory#create (Parameter { typ = `Status }) in
  let pr      = factory#create (Parameter { typ = `Protocol }) in
  let psz     = factory#create (Parameter { typ = `Packet_size }) in
  let (items : 'a Dashboard.Item.positioned_item list) =
    [ { item = in_br;   position = { x=0;y=0;w=1;h=1} }
    ; { item = jt   ;   position = { x=1;y=0;w=1;h=1} }
    ; { item = tpperip; position = { x=2;y=0;w=1;h=1} }
    ; { item = status;  position = { x=3;y=0;w=1;h=1} }
    ; { item = pr;      position = { x=0;y=1;w=1;h=1} }
    ; { item = psz;     position = { x=1;y=1;w=1;h=1} }
    ]
  in
  let dg = new Dashboard.t ~items () in
  object
    inherit Widget.widget dg#root ()
    method on_load   = dg#layout
    method on_unload = factory#destroy
  end
