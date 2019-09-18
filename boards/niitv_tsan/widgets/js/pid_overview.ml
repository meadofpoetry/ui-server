open Js_of_ocaml
open Js_of_ocaml_tyxml
open Application_types
open Board_niitv_tsan_types
open Components
include Board_niitv_tsan_widgets_tyxml.Pid_overview
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

type event =
  [ `State of [Topology.state | `No_sync]
  | `Bitrate of Bitrate.ext option
  | `PIDs of (int * PID.t) list ts ]

module Set = Set.Make (struct
  type t = int * PID.t

  let compare (a : t) (b : t) : int = Int.compare (fst a) (fst b)
end)

let update_row_bitrate
    (table : 'a Gadt_data_table.t)
    (total : Bitrate.value)
    (br : Bitrate.value)
    row =
  let pct = 100. *. float_of_int br.cur /. float_of_int total.cur in
  let mbps = float_of_int br.cur /. 1_000_000. in
  let data =
    Gadt_data_table.Fmt_js.
      [ None
      ; None
      ; None
      ; None
      ; Some (Some mbps)
      ; Some (Some pct)
      ; Some (Some (float_of_int br.min /. 1_000_000.))
      ; Some (Some (float_of_int br.max /. 1_000_000.)) ]
  in
  table#set_row_data_some data row

let update_row_info (table : 'a Gadt_data_table.t) row pid (info : PID.t) =
  let flags = {has_pcr = info.has_pcr; scrambled = info.scrambled} in
  Element.toggle_class_unit ~force:(not info.present) row CSS.row_lost;
  let data =
    Gadt_data_table.Fmt_js.
      [ Some pid
      ; Some info.typ
      ; Some flags
      ; Some info.service_name
      ; None
      ; None
      ; None
      ; None ]
  in
  table#set_row_data_some data row

class t ?(init : (int * PID.t) list ts option) (elt : Dom_html.element Js.t) () =
  object (self)
    val mutable data : Set.t =
      Set.of_list
        (match init with
        | None -> []
        | Some {data; _} -> data)

    inherit
      [int] Table_overview.t ~format:(Markup_js.create_table_format ()) elt () as super

    method pids : (int * PID.t) list = List.of_seq @@ Set.to_seq data

    method notify : event -> unit =
      function
      | `State x -> super#set_state x
      | `PIDs x -> self#set_pids x
      | `Bitrate x -> self#set_bitrate x

    method set_bitrate : Bitrate.ext option -> unit =
      function
      | None -> () (* FIXME do smth *)
      | Some {total; pids; _} ->
          List.iter
            (fun (pid, br) ->
              let row = self#find_row pid in
              Option.iter (update_row_bitrate table total br) row)
            pids
    (** Updates bitrate values *)

    method set_pids (pids : (int * PID.t) list ts) =
      (* Manage found, lost and updated items *)
      let old = data in
      let cur = Set.of_list pids.data in
      data <- cur;
      (* Handle lost PIDs *)
      Set.iter (super#remove_row % fst) @@ Set.diff old cur;
      (* Handle found PIDs *)
      Set.iter self#add_pid @@ Set.diff cur old;
      (* Update existing PIDs *)
      Set.iter self#update_pid @@ Set.inter cur old;
      self#update_empty_state ()

    method private update_pid (pid, info) =
      match self#find_row pid with
      | None -> ()
      | Some row -> update_row_info table row pid info

    method private add_pid x =
      let (data : _ Markup_js.Fmt.data) = Markup_js.data_of_pid_info x in
      let row = table#insert_row (-1) data in
      Element.toggle_class_unit ~force:(not (snd x).present) row CSS.row_lost

    method private find_row (pid : int) =
      let find row =
        let pid' =
          Gadt_data_table.Fmt_js.(
            match table#get_row_data_lazy row with
            | pid :: _ -> pid ())
        in
        pid = pid'
      in
      List.find_opt find table#rows

    method private set_hex (hex : bool) : unit =
      let pid_fmt = Markup_js.pid_fmt ~hex in
      let (format : _ Markup_js.Fmt.data_format) =
        match table#data_format with
        | _ :: tl -> pid_fmt :: tl
      in
      table#set_data_format ~redraw:false format;
      List.iter
        (fun row ->
          let cells = row##.cells in
          Js.Opt.iter
            (cells##item 0)
            (fun cell ->
              Gadt_data_table.(set_cell_value pid_fmt (get_cell_value pid_fmt cell) cell)))
        table#rows
  end

let attach ?init elt : t = new t ?init (elt : Dom_html.element Js.t) ()

let make ?classes ?attrs ?dense ?init ~control () =
  Markup_js.create ?classes ?attrs ?dense ?init ~control ()
  |> Tyxml_js.To_dom.of_div
  |> attach ?init
