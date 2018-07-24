open Containers
open Components
open Common
open Lwt_result
open Api_js.Api_types
open Board_types.Streams.TS

type time = [ `Now | `Last of Time.t ]

module Stream_item = struct

  let base_class = "qos-niit-stream-grid-item"
  let lost_class = Markup.CSS.add_modifier base_class "lost"

  let time_to_string = function
    | `Now    -> "Активен"
    | `Last t ->
       let tz_offset_s = Ptime_clock.current_tz_offset_s () in
       Time.to_human_string ?tz_offset_s t

  class t (time:time)
          (stream:Stream.t)
          (event:Stream.t option React.event) () =
    let descr    = Option.get_or ~default:"Нет описания" stream.description in
    let title    = new Card.Primary.title descr () in
    let subtitle = new Card.Primary.subtitle (time_to_string time) () in
    let primary  = new Card.Primary.t ~widgets:[ title; subtitle ] () in
    object(self)
      inherit Card.t ~widgets:[ primary#widget ] ()
      inherit Widget.stateful ()

      method stream = stream

      initializer
        React.E.map (function
            | Some _ ->
               self#remove_class lost_class;
               subtitle#set_text_content @@ time_to_string `Now;
            | None   ->
               self#add_class lost_class;
               let time = time_to_string @@ `Last (Time.Clock.now_s ()) in
               subtitle#set_text_content time) event
        |> self#_keep_e;
        (match time with
         | `Last _ -> self#add_class lost_class
         | _ -> ())
    end

end

module Stream_grid = struct

  let base_class = "qos-niit-stream-grid"

  class t (init:(Stream.t * time) list)
          (event:Stream.t list React.event)
          () =
    let ph = Ui_templates.Placeholder.create_with_icon
               ~icon:"info"
               ~text:"Не найдено ни одного потока"
               () in
    object(self)
      val mutable _streams = []

      inherit Hbox.t ~wrap:`Wrap ~widgets:[] ()
      inherit Widget.stateful ()

      method streams =
        List.map (fun x -> x#stream) _streams

      method add_stream ?(time=`Now) (stream:Stream.t) =
        let e = React.E.map (List.find_opt (Stream.equal stream)) event
                |> React.E.changes ~eq:(Equal.option (fun _ _ -> false)) in
        let w = new Stream_item.t time stream e () in
        _streams <- w :: _streams;
        if List.is_empty _widgets
        then Dom.removeChild self#root ph#root;
        Dom.appendChild self#root w#root

      initializer
        self#add_class base_class;
        List.iter (fun (s, time) -> self#add_stream ~time s) init;
        if List.is_empty _streams
        then Dom.appendChild self#root ph#root;
        React.E.map (fun l ->
            let added =
              List.filter (fun x ->
                  not @@ List.mem ~eq:Stream.equal x self#streams) l in
            List.iter self#add_stream added) event
        |> self#_keep_e
    end

end

let make input control =
  let streams =
    Requests.Streams.HTTP.get_streams
      ~inputs:[input]
      ~compress:true
      control
    >>= (function
         | Compressed x -> Lwt_result.return x.data
         | _            -> Lwt.fail_with "raw")
    |> Lwt_result.map_err Api_js.Requests.err_to_string in
  streams
  >|= (fun streams ->
    let event, sock =
      Requests.Streams.WS.get_streams ~inputs:[input] control in
    let grid = new Stream_grid.t streams event () in
    grid#widget)
  |> Ui_templates.Loader.create_widget_loader
  |> Widget.coerce
