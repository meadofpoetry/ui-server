open Containers
open Components
open Common

let make (stream : Stream.ID.t) (control:int) =
  let factory = new Widget_factory.t control () in
  let duration = Common.Time.Span.of_int_s 120 in
  let conf t =
    (Some { typ = t
          ; ids = [stream]
          ; duration
          ; settings = None } : Widget_chart.config option) in
  let default : Widget_factory.item Dashboard.Item.positioned_item list =
    [ { item = Stream_chart (conf `Power)
      ; position = { x = 0; y = 0; w = 4; h = 2 }
      }
    ; { item = Stream_chart (conf `Mer)
      ; position = { x = 0; y = 2; w = 4; h = 2 }
      }
    ; { item = Stream_chart (conf `Ber)
      ; position = { x = 0; y = 4; w = 4; h = 2 }
      }
    ; { item = Stream_chart (conf `Freq)
      ; position = { x = 0; y = 6; w = 4; h = 2 }
      }
    ; { item = Stream_chart (conf `Bitrate)
      ; position = { x = 0; y = 8; w = 4; h = 2 }
      }
    ]
  in
  new Dashboard.t
    ~non_editable:true
    ~items:default
    factory
    ()
