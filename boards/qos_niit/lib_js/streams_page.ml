open Containers
open Components
open Common

module Stream_item = struct

  let id_to_string = function
    | `Ts id -> Stream.id_to_int32 id |> Int32.to_string
    | `Ip ip -> Url.to_string ip

  class t (stream:Stream.t) () =
    let descr    = Option.get_or ~default:"Нет описания" stream.description in
    let title    = new Card.Primary.title descr () in
    let id       = "id: " ^ (id_to_string stream.id) in
    let subtitle = new Card.Primary.subtitle id () in
    let primary  = new Card.Primary.t ~widgets:[ title; subtitle ] () in
    object
      inherit Card.t ~widgets:[ primary#widget
                              ; (new Divider.t ())#widget ] ()
    end

end

let make () =
  let streams : Stream.t list =
    [ { source      = Input { input = RF; id = 1 }
      ; id          = `Ts (Dvb (2, 0))
      ; description = Some "TS 1"
      }
    ; { source      = Input { input = RF; id = 1 }
      ; id          = `Ts (Dvb (3, 1))
      ; description = Some "TS 1"
      }
    ; { source      = Input { input = RF; id = 1 }
      ; id          = `Ts (Dvb (4, 2))
      ; description = Some "TS 1"
      }
    ; { source      = Input { input = RF; id = 1 }
      ; id          = `Ts (Dvb (5, 0))
      ; description = Some "TS 1"
      }
    ] in
  let cards = List.map (fun s -> new Stream_item.t s ()) streams in
  let cells = List.map (fun c ->
                  let cell = new Layout_grid.Cell.t ~widgets:[c] () in
                  cell#set_span_desktop @@ Some 3;
                  cell#set_span_phone   @@ Some 12;
                  cell#set_span_tablet  @@ Some 4;
                  cell)
                cards in
  let grid  = new Layout_grid.t ~cells () in
  grid#widget
