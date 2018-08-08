open Containers
open Components
open Common
open Lwt_result
open Api_js.Api_types
open Board_types.Streams.TS

type time = [ `Now | `Last of Time.t ]

let compare_time (x:time) (y:time) = match x, y with
  | `Now, `Now       -> 0
  | `Last x, `Last y -> Time.compare x y
  | `Now, `Last _    -> -1
  | `Last _, `Now    -> 1

module Stream_item = struct

  let base_class = "qos-niit-stream-grid-item"
  let lost_class = Markup.CSS.add_modifier base_class "lost"

  let time_to_string = function
    | `Now    -> "Активен"
    | `Last t ->
       let tz_offset_s = Ptime_clock.current_tz_offset_s () in
       Time.to_human_string ?tz_offset_s t

  let typ_to_string = function
    | `Ts   -> "MPEG-TS"
    | `T2mi -> "T2-MI"

  let source_to_string (source:Stream.source) : string =
    let rec aux acc : Stream.source -> string list =
      function
      | Input i -> ("Вход " ^ Topology.get_input_name i) :: acc
      | Parent (stream:Stream.t) ->
         let s = "Поток " ^ (match stream.description with
                             | None   -> "без описания"
                             | Some s -> s) in
         aux (s :: acc) stream.source in
    String.concat " -> " @@ aux [] source

  class t on_lost on_found
          (time:time)
          (stream:Stream.t)
          (event:Stream.t option React.event)
          control () =
    let descr = Option.get_or ~default:"Нет описания" stream.description in
    (* FIXME change to stream type *)
    let title =
      new Card.Primary.title ~large:true (typ_to_string stream.typ) () in
    let description =
      new Card.Primary.subtitle descr () in
    let primary =
      new Card.Primary.t ~widgets:[ title; description ] () in
    let source =
      new Typography.Text.t
        ~font:Caption ~adjust_margin:false
        ~text:(source_to_string stream.source) () in
    let timestamp =
      new Typography.Text.t
        ~font:Caption ~adjust_margin:false
        ~text:(time_to_string time) () in
    let media =
      new Card.Media.t ~widgets:[ source#widget
                                ; timestamp#widget ] () in
    object(self)

      val mutable _time = time

      inherit Card.t ~widgets:[ primary#widget
                              ; (new Divider.t ())#widget
                              ; media#widget ] ()

      method stream = stream
      method time   = _time

      initializer
        self#add_class base_class;
        Dom_events.listen self#root Dom_events.Typ.click (fun _ _ ->
            Stream_page.make stream control |> ignore;
            print_endline "clicked"; true) |> ignore;
        React.E.map (function
            | Some _ ->
               on_found self;
               self#remove_class lost_class;
               _time <- `Now;
               timestamp#set_text_content @@ time_to_string `Now;
            | None   ->
               on_lost self;
               self#add_class lost_class;
               (* This time may differ from the time detected on a server-side.
                  Maybe do a request? *)
               let time = `Last (Time.Clock.now_s ()) in
               _time <- time;
               timestamp#set_text_content @@ time_to_string time) event
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
          control
          () =
    let ph =
      Ui_templates.Placeholder.create_with_icon
        ~icon:"info"
        ~text:"Не найдено ни одного потока"
        () in
    let title         = new Typography.Text.t ~text:"Текущие потоки" () in
    let grid          = new Layout_grid.t ~cells:[] () in
    let archive_title = new Typography.Text.t ~text:"Архив потоков" () in
    let archive_grid  = new Layout_grid.t ~cells:[] () in
    object(self)

      val mutable _archive_cells = []
      val mutable _current_cells = []

      inherit Vbox.t ~widgets:[ title#widget
                              ; grid#widget
                              ; archive_title#widget
                              ; archive_grid#widget ] ()

      method cells = _archive_cells @ _current_cells

      method streams =
        List.map (fun x -> x#stream) self#cells

      method add_stream ?(time=`Now) (stream:Stream.t) =
        let e = React.E.map (List.find_opt (Stream.equal stream)) event
                |> React.E.changes ~eq:(Equal.option (fun _ _ -> false)) in
        let w =
          new Stream_item.t self#_on_lost self#_on_found
            time stream e control () in
        if List.is_empty self#cells
        then self#remove_child ph;
        let cell = new Layout_grid.Cell.t ~widgets:[w] () in
        cell#set_span_desktop @@ Some 3;
        cell#set_span_tablet  @@ Some 4;
        cell#set_span_phone   @@ Some 12;
        match time with
        | `Now ->
           _current_cells <- w :: _current_cells;
           grid#inner#append_child cell
        | `Last _ ->
           _archive_cells <- w :: _archive_cells;
           archive_grid#inner#append_child cell

      method private _check_and_rm_current () =
        if List.is_empty _current_cells
        then (self#remove_child title;
              self#remove_child grid)

      method private _check_and_rm_archive () =
        if List.is_empty _current_cells
        then (self#remove_child title;
              self#remove_child grid)

      method private _on_lost = fun w ->
        grid#remove_child w;
        archive_grid#insert_child_at_idx 0 w;
        if List.is_empty _archive_cells
        then (self#insert_child_at_idx 2 archive_title;
              self#insert_child_at_idx 3 archive_grid);
        _archive_cells <- List.add_nodup
                            ~eq:Equal.physical
                            w
                            _archive_cells;
        _current_cells <- List.remove
                            ~eq:Equal.physical
                            ~x:w
                            _current_cells;
        self#_check_and_rm_current ()

      method private _on_found = fun w ->
        archive_grid#remove_child w;
        grid#insert_child_at_idx 0 w;
        if List.is_empty _current_cells
        then (self#insert_child_at_idx 2 archive_title;
              self#insert_child_at_idx 3 archive_grid);
        _current_cells <- List.add_nodup
                            ~eq:Equal.physical
                            w
                            _current_cells;
        _archive_cells <- List.remove
                            ~eq:Equal.physical
                            ~x:w
                            _archive_cells;
        self#_check_and_rm_current ()

      initializer
        self#add_class base_class;
        if List.is_empty init
        then self#append_child ph
        else List.iter (fun (s, time) -> self#add_stream ~time s)
             @@ List.sort (Ord.pair Stream.compare compare_time) init;
        self#_check_and_rm_current ();
        self#_check_and_rm_archive ();
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
    let grid = new Stream_grid.t streams event control () in
    grid#widget)
  |> Ui_templates.Loader.create_widget_loader
  |> Widget.coerce
