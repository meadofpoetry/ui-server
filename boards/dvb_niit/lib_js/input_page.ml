open Containers
open Components

let make (control:int) =
  let factory = new Widget_factory.t control () in
  let duration = Common.Time.Span.of_int_s 120 in
  let conf t =
    (Some { typ = t
          ; ids = [0; 1; 2; 3]
          ; duration
          ; settings = None } : Widget_chart.config option) in
  let default : Widget_factory.item Dashboard.Item.positioned_item list =
    [ { item = Chart (conf `Power)
      ; position = { x = 0; y = 0; w = 4; h = 2 }
      }
    ; { item = Chart (conf `Mer)
      ; position = { x = 0; y = 2; w = 4; h = 2 }
      }
    ; { item = Chart (conf `Ber)
      ; position = { x = 0; y = 4; w = 4; h = 2 }
      }
    ; { item = Chart (conf `Freq)
      ; position = { x = 0; y = 6; w = 4; h = 2 }
      }
    ; { item = Chart (conf `Bitrate)
      ; position = { x = 0; y = 8; w = 4; h = 2 }
      }
    ]
  in
  let dashboard = new Dashboard.t ~items:default factory () in
  dashboard
