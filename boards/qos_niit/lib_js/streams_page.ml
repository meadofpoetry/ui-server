open Containers
open Components
open Common
open Lwt_result
open Api_js.Api_types
open Board_types.Streams.TS

type time = [ `Now | `Last of Time.t ]

module Stream_item = struct

  let base_class = "qos-niit-stream-item"
  let lost_class = Markup.CSS.add_modifier base_class "lost"

  let id_to_string = function
    | `Ts id -> Stream.id_to_int32 id |> Int32.to_string
    | `Ip ip -> Url.to_string ip

  let time_to_string = function
    | `Now    -> "есть"
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
    let event, sock = Requests.Streams.WS.get_streams
                        ~inputs:[input]
                        control in
    let cards =
      List.map (fun (s, time) ->
          let e = React.E.map (List.find_opt (fun x -> Stream.equal s x))
                    event
                  |> React.E.changes ~eq:(fun x y -> match x, y with
                                                     | Some _, None -> false
                                                     | None, Some _ -> false
                                                     | _ -> true) in
          new Stream_item.t time s e ())
        streams in
    let grid  = new Hbox.t ~wrap:`Wrap ~widgets:cards () in
    grid#widget)
  |> Ui_templates.Loader.create_widget_loader
  |> Widget.coerce
