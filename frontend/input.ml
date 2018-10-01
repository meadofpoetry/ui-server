open Containers
open Components
open Api_js.Requests
open Api_js.Api_types
open Lwt_result.Infix
open Common.Topology
open Common

type time = [ `Now | `Last of Time.t ]

module WS = struct

  type e =
    { event : Stream.t list React.event
    ; close : unit -> unit
    }

  let now input board control =
    match board with
    | "TS" ->
       Board_qos_niit_js.Requests.Streams.WS.get_streams
         ~inputs:[input]
         control
       |> fun (e, s) ->
          Some { event = e; close = fun () -> s##close }
    | _ -> Some { event = React.E.never
                ; close = fun () -> () }

end

module HTTP = struct

  let history input board control =
    match board with
    | "TS" ->
       Board_qos_niit_js.Requests.History.HTTP.Streams.get
         ~compress:true
         ~inputs:[input]
         control
       >>= (function
            | Compressed x -> Lwt_result.return x.data
            | _ -> Lwt.fail_with "raw")
    | _ -> Lwt_result.return []

  let now input board control =
    match board with
    | "TS" ->
       Board_qos_niit_js.Requests.Streams.HTTP.get_streams
         ~inputs:[input]
         control
    | _ -> Lwt_result.return []

end

let compare_time (x : time) (y : time) = match x, y with
  | `Now, `Now -> 0
  | `Last x, `Last y -> Time.compare x y
  | `Now, `Last _ -> -1
  | `Last _, `Now -> 1

module Stream_item = struct

  let base_class = "qos-niit-stream-grid-item"
  let lost_class = Markup.CSS.add_modifier base_class "lost"

  let time_to_string = function
    | `Now    -> "Активен"
    | `Last t ->
       let tz_offset_s = Ptime_clock.current_tz_offset_s () in
       Time.to_human_string ?tz_offset_s t

  let typ_to_string : Stream.stream_type -> string = function
    | TS   -> "MPEG-TS"
    | T2MI -> "T2-MI"

  let source_to_string (source : Stream.source) : string =
    let rec aux acc (source : Stream.source) =
      let open Topology in
      match source.node with
      | Entry (Input i) -> ("Вход "  ^ Topology.get_input_name i) :: acc
      | Entry (Board b) -> ("Плата " ^ b.model) :: acc
      | Stream (stream : Stream.t) ->
         let s = "Поток " ^ (Stream.Source.to_string stream.source.info) in
         aux (s :: acc) stream.source in
    String.concat " -> " @@ aux [] source

  class t on_lost on_found
          (input : topo_input)
          (time : time)
          (stream : Stream.t)
          (event : Stream.t option React.event)
          () =
    let descr = Stream.Source.to_string stream.source.info in
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
        self#listen_lwt Widget.Event.click (fun _ _ ->
            let id = Stream.ID.to_string self#stream.id in
            let input_name = input_to_string input.input in
            let url = Printf.sprintf "/input/%s/%d/%s"
                        input_name input.id id in
            Dom_html.window##.location##.href := Js.string url;
            Lwt.return_unit) |> Lwt.ignore_result;
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

let find_cell (w : Stream_item.t)
      (cells : Layout_grid.Cell.t list) =
  let f : Widget.t list -> bool = function
    | [wdg] -> Widget.equal w#widget wdg
    | _ -> false in
  List.find_opt (fun x -> f x#widgets) cells

module Stream_grid = struct

  let base_class = "qos-niit-stream-grid"

  class t (input : topo_input)
          (init : (Stream.t * time) list)
          (event : Stream.t list React.event)
          () =
    let ph =
      Ui_templates.Placeholder.create_with_icon
        ~icon:Icon.SVG.(create_simple Path.information)
        ~text:"Не найдено ни одного потока"
        () in
    let title         = new Typography.Text.t ~text:"Текущие потоки" () in
    let grid          = new Layout_grid.t ~cells:[] () in
    let archive_title = new Typography.Text.t ~text:"Архив потоков" () in
    let archive_grid  = new Layout_grid.t ~cells:[] () in
    object(self)

      val mutable _streams : Stream_item.t list = []

      inherit Vbox.t ~widgets:[ title#widget
                              ; grid#widget
                              ; archive_title#widget
                              ; archive_grid#widget ] ()

      method cells   = archive_grid#cells @ grid#cells
      method streams = List.map (fun x -> x#stream) _streams

      method add_stream ?(time=`Now) (stream:Stream.t) =
        let e = React.E.map (List.find_opt (Stream.equal stream)) event
                |> React.E.changes ~eq:(Equal.option (fun _ _ -> false)) in
        let w =
          new Stream_item.t self#_on_lost self#_on_found input
            time stream e () in
        if List.is_empty self#cells
        then self#remove_child ph;
        let cell =
          new Layout_grid.Cell.t
            ~span_desktop:3
            ~span_tablet:4
            ~span_phone:12
            ~widgets:[w] () in
        _streams <- w :: _streams;
        match time with
        | `Now    ->
           if List.is_empty self#cells
           then (self#insert_child_at_idx 0 title;
                 self#insert_child_at_idx 1 grid);
           grid#append_cell cell;
        | `Last _ ->
           if List.is_empty self#cells
           then (self#insert_child_at_idx 2 archive_title;
                 self#insert_child_at_idx 3 archive_grid);
           archive_grid#append_cell cell

      method private _check_and_rm_current () =
        if List.is_empty grid#cells
        then (self#remove_child title;
              self#remove_child grid)

      method private _check_and_rm_archive () =
        if List.is_empty archive_grid#cells
        then (self#remove_child archive_title;
              self#remove_child archive_grid)

      method private _on_lost = fun w ->
        match find_cell w grid#cells with
        | Some cell ->
           grid#remove_cell cell;
           if List.is_empty archive_grid#cells
           then (self#insert_child_at_idx 2 archive_title;
                 self#insert_child_at_idx 3 archive_grid);
           archive_grid#insert_cell_at_idx 0 cell;
           self#_check_and_rm_current ()
        | _ -> ()

      method private _on_found = fun w ->
        match find_cell w archive_grid#cells with
        | Some cell ->
           archive_grid#remove_cell cell;
           if List.is_empty grid#cells
           then (self#insert_child_at_idx 0 title;
                 self#insert_child_at_idx 1 grid);
           grid#insert_cell_at_idx 0 cell;
           self#_check_and_rm_archive ()
        | _ -> ()

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

let get_streams_ws input (boards : (int * string) list) =
  List.map (fun (control, board) ->
      WS.now input board control) boards
  |> List.all_some
  |> function
    | Some l ->
       let ev, s =
         List.map (fun (x : WS.e) -> x.event, x.close) l
         |> List.split in
       let e = React.E.merge (fun _ x -> x) [] ev in
       let s = fun () -> List.iter (fun f -> f ()) s in
       e, s
    | None -> React.E.never, fun () -> ()

let get_streams input (boards : (int * string) list) =
  let open Lwt.Infix in
  Lwt_list.map_p (fun (control, board) ->
      HTTP.history input board control) boards
  >|= List.all_ok
  >|= function
  | Ok l -> Ok (List.flatten l)
  | Error e -> Error e

let make () =
  let (input : topo_input) =
    Js.Unsafe.global##.input
    |> Js.to_string
    |> Show_topo_input.of_string in
  let (boards : (int * string) list) =
    Yojson.Safe.from_string @@ Js.to_string
    @@ Js_of_ocaml.Json.output @@ Js.Unsafe.global##.boards
    |> boards_of_yojson
    |> Result.get_exn in
  let (cpu : string option) =
    Yojson.Safe.from_string @@ Js.to_string
    @@ Js_of_ocaml.Json.output @@ Js.Unsafe.global##.cpu
    |> cpu_opt_of_yojson
    |> Result.get_exn in
  let streams =
    get_streams input boards
    |> Lwt_result.map_err Api_js.Requests.err_to_string in
  let w =
    streams
    >|= (fun streams ->
      let event, close =
        get_streams_ws input boards in
      let grid = new Stream_grid.t input streams event () in
      grid#widget)
    |> Ui_templates.Loader.create_widget_loader
    |> Widget.coerce in
  let _ = new Ui_templates.Page.t (`Static [w]) () in
  ()

let () = make ()
