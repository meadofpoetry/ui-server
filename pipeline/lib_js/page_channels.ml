open Containers
open Components
open Common

let make (id : Stream.ID.t) () =
  let factory = new Widget_factory.t () in
  let config typ : Widget_parameter_chart.widget_config =
    { duration = Time.Span.of_int_s 120
    ; typ
    ; sources = []
    ; settings = None
    } in
  let (default : Widget_factory.item Dashboard.Item.positioned_item list) =
    [ { item = Chart (Some (config `Black))
      ; position = { x = 0; y = 0; w = 4; h = 3 }
      }
    ; { item = Chart (Some (config `Freeze))
      ; position = { x = 0; y = 3; w = 4; h = 3 }
      }
    ; { item = Chart (Some (config `Blocky))
      ; position = { x = 0; y = 6; w = 4; h = 3 }
      }
    ] in
  new Dashboard.t
    ~non_editable:true
    ~items:default
    factory
    ()
