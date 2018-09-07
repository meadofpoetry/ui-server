open Containers
open Components
open Common
open Board_types.Streams.T2MI
open Widget_common
open Lwt_result.Infix

let base_class = "qos-niit-t2mi-sequence"

module Heading = struct

  class t ?title ?subtitle () =
    let title' = new Card.Primary.title ~large:true "" () in
    let subtitle' = new Card.Primary.subtitle "" () in
    let box = Widget.create_div ~widgets:[title'; subtitle'] () in
    object(self)
      inherit Card.Primary.t ~widgets:[box] ()

      method set_title (s : string) : unit =
        title'#set_text_content s

      method set_subtitle (s : string) : unit =
        subtitle'#set_text_content s

      initializer
        Option.iter self#set_title title;
        Option.iter self#set_subtitle subtitle;
    end

end

module Sequence = struct

  let packet_type =
    let open Table in
    { is_numeric = false
    ; compare = Int.compare
    ; to_string =
        function
        | 0x00 -> "BB frame"
        | 0x01 -> "Auxiliary stream I/Q data"
        | 0x02 -> "Arbitrary cell insertion"
        | 0x10 -> "L1 current"
        | 0x11 -> "L1 future"
        | 0x12 -> "P2 bias balancing cells"
        | 0x20 -> "DVB-T2 timestamp"
        | 0x21 -> "Individual addressing"
        | 0x30 -> "FEF part : Null"
        | 0x31 -> "FEF part : I/Q data"
        | 0x32 -> "FEF part : composite"
        | 0x33 -> "FEF sub-part"
        | x -> Printf.sprintf "Reserved for future use (%d)" x
    }

  let dec_fmt = Table.(Int None)
  let hex_fmt = Table.(Int (Some (Printf.sprintf "0x%X")))

  let extra_fmt ~is_hex =
    let open Table in
    Custom { is_numeric = false
           ; compare = (fun _ _ -> 0)
           ; to_string =
               (fun i ->
                 let to_string =
                   if is_hex
                   then Printf.sprintf "0x%X"
                   else Printf.sprintf "%d" in
                 match i.typ with
                 | 0x00 ->
                    Printf.sprintf "PLP %s" @@ to_string i.plp
                 | 0x10 ->
                    Printf.sprintf "L1DYN_CURR.FRAME_IDX = %s"
                    @@ to_string i.l1_param_1
                 | 0x11 ->
                    Printf.sprintf "L1DYN_NEXT.FRAME_IDX = %s, \
                                    L1DYN_NEXT2.FRAME_IDX = %s"
                      (to_string i.l1_param_1)
                      (to_string i.l1_param_2)
                 | _ -> "")
      }

  let fmt is_hex =
    let open Table in
    let open Format in
    let int_fmt = if is_hex then hex_fmt else dec_fmt in
    (to_column "Тип пакета", Custom packet_type)
    :: (to_column "Кол-во пакетов", Int None)
    :: (to_column "T2-MI Stream ID", int_fmt)
    :: (to_column "frame", Option (int_fmt, ""))
    :: (to_column "super-frame", int_fmt)
    :: (to_column "Подробности", extra_fmt ~is_hex)
    :: []

  class ['a] t is_hex () =
    let icon = Icon.SVG.(create_simple Path.download) in
    let ph =
      Ui_templates.Placeholder.create_with_icon
        ~text:"Для получения последовательности пакетов T2-MI,\n \
               нажмите кнопку \"Загрузить\""
        ~icon
        () in
    let table =
      new Table.t
        ~sticky_header:true
        ~dense:true
        ~fmt:(fmt is_hex) () in
    object(self)

      val mutable _is_hex = is_hex

      inherit Card.Media.t ~widgets:[table] ()

      method set_hex (x : bool) : unit =
        _is_hex <- x;
        List.iter (fun (row : 'a Table.Row.t) ->
            let open Table in
            match row#cells with
            | _ :: _ :: sid :: frame :: sframe :: extra :: [] ->
               let fmt = if x then hex_fmt else dec_fmt in
               sid#set_format fmt;
               frame#set_format (Option (fmt, ""));
               sframe#set_format fmt;
               extra#set_format (extra_fmt ~is_hex:x)) table#rows

      method set_lwt (t : (sequence, string) Lwt_result.t) =
        Lwt.try_bind
          (fun () -> t)
          (fun r ->
            match r with
            | Ok x ->
               table#remove_all_rows ();
               begin match x.items with
               | [] ->
                  ph#set_text "Не обнаружено пакетов T2-MI!";
                  icon#path#set Icon.SVG.Path.information;
                  ph#remove_class Ui_templates.Placeholder.with_error_class;
                  self#_show_placeholder ()
               | x ->
                  List.iter self#_add_row x;
                  self#_hide_placeholder ()
               end;
               Lwt.return_unit
            | Error e ->
               self#_on_error e;
               Lwt.return_unit)
          (fun e ->
            self#_on_error @@ Printexc.to_string e;
            Lwt.return_unit)

      (* Private methods *)

      method private _on_error (e : string) =
        ph#set_text e;
        icon#path#set Ui_templates.Placeholder.error_svg_path;
        ph#add_class Ui_templates.Placeholder.with_error_class;
        self#_show_placeholder ()

      method private _hide_all () =
        ph#style##.display := Js.string "none";
        table#style##.display := Js.string "none"

      method private _hide_placeholder () =
        ph#style##.display := Js.string "none";
        table#style##.display := Js.string ""

      method private _show_placeholder () =
        ph#style##.display := Js.string "";
        table#style##.display := Js.string "none"

      method private _add_row (i : sequence_item) : unit =
        let open Table.Data in
        let frame = match i.typ with
          | 0x00 | 0x01 | 0x02 -> Some i.frame
          | 0x10 | 0x11 | 0x12 -> Some i.frame
          | _ -> None in
        let data =
          i.typ :: i.count :: i.stream_id
          :: frame :: i.super_frame :: i :: [] in
        table#add_row data |> ignore

      initializer
        self#append_child ph;
        if table#is_empty then self#_show_placeholder ()

    end

end

let make_select () =
  let items =
    List.map (fun x ->
        `Item (new Select.Item.t
                 ~value:x
                 ~text:(Printf.sprintf "%g сек" x)
                 ())) [5.; 10.; 30.] in
  new Select.t
    ~label:"Длительность"
    ~default_selected:true
    ~items
    ()

class t (stream : Stream.t) (control : int) () =
  (* FIXME read from storage *)
  let is_hex = false in
  let title = "Последовательность пакетов" in
  let subtitle = make_timestamp_string None in
  let select = make_select () in
  let primary = new Heading.t ~title ~subtitle () in
  let button =
    new Ui_templates.Buttons.Get.t
      ~style:`Raised
      ~label:"Загрузить"
      () in
  let sequence = new Sequence.t is_hex () in
  let getter () =
    let duration =
      Option.get_exn
      @@ React.S.value
      @@ select#s_selected_value in
    Requests.Streams.HTTP.T2MI.get_sequence
      ~id:stream.id control
      ~duration:(Option.get_exn @@ Time.Span.of_float_s duration)
    |> Lwt_result.map_err Api_js.Requests.err_to_string
    >|= (fun x ->
      primary#set_subtitle @@ make_timestamp_string @@ Some x.timestamp;
      x)
    |> sequence#set_lwt in
  let on_change = fun (x : bool) -> sequence#set_hex x in
  let buttons =
    new Card.Actions.Buttons.t
      ~widgets:[button]
      () in
  let actions =
    new Card.Actions.t
      ~widgets:[buttons; select#widget]
      () in
  let switch =
    new Switch.t
      ~state:is_hex
      ~on_change
      () in
  let hex =
    new Form_field.t
      ~input:switch
      ~label:"HEX IDs"
      () in
  object(self)

    inherit Card.t ~widgets:[] ()

    initializer
      self#_keep_s
      @@ React.S.map button#set_timeout
      @@ select#s_selected_value;
      button#set_getter @@ Some getter;
      primary#append_child hex;
      self#add_class base_class;
      self#append_child primary;
      self#append_child @@ new Divider.t ();
      self#append_child actions;
      self#append_child @@ new Divider.t ();
      self#append_child sequence;

  end

let make (stream : Stream.t) (control : int) =
  new t stream control ()
