open Containers
open Components

let make (control : int) =
  let factory = new Widget_factory.t control () in
  let (items : Widget_factory.item Dashboard.Item.positioned_item list) =
    [ { item = Parameter { typ = `Input_bitrate }
      ; position = { x=0;y=0;w=1;h=1} }
    ; { item = Parameter { typ = `Jitter_tolerance }
      ; position = { x=1;y=0;w=1;h=1} }
    ; { item = Parameter { typ = `TP_per_IP }
      ; position = { x=2;y=0;w=1;h=1} }
    ; { item = Parameter { typ = `Status }
      ; position = { x=3;y=0;w=1;h=1} }
    ; { item = Parameter { typ = `Protocol }
      ; position = { x=0;y=1;w=1;h=1} }
    ; { item = Parameter { typ = `Packet_size }
      ; position = { x=1;y=1;w=1;h=1} }
    ]
  in
  new Dashboard.t ~items factory ()
