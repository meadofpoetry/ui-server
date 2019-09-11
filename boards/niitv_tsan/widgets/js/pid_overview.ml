open Js_of_ocaml
open Js_of_ocaml_tyxml
open Application_types
open Board_niitv_tsan_types
open Components
include Board_niitv_tsan_widgets_tyxml.Pid_overview
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

type event =
  [ `State of [Topology.state | `No_sync]
  | `Bitrate of Bitrate.t option
  | `PIDs of (int * PID_info.t) list ts ]

module Attr = struct
  let data_lost = "data-lost"

  let set_bool elt attr = function
    | true -> Element.set_attribute elt attr ""
    | false -> Element.remove_attribute elt attr
end

module Selector = struct
  let table = "." ^ CSS.table

  let placeholder = "." ^ Components_lab.Placeholder.CSS.root
end

module Pid_info = struct
  type t = int * PID_info.t

  let compare (a : t) (b : t) : int = Int.compare (fst a) (fst b)
end

module Set = Set.Make (Pid_info)

let update_row_bitrate (table : 'a Gadt_data_table.t) total br row =
  let pct = 100. *. float_of_int br /. float_of_int total in
  let br = float_of_int br /. 1_000_000. in
  let min, max =
    Gadt_data_table.Fmt_js.(
      match table#get_row_data_lazy row with
      | [_; _; _; _; _; _; min; max] -> min, max)
  in
  let min =
    match min () with
    | None -> Some (Some br)
    | Some v -> if br < v then Some (Some br) else None
  in
  let max =
    match max () with
    | None -> Some (Some br)
    | Some v -> if br > v then Some (Some br) else None
  in
  let data =
    Gadt_data_table.Fmt_js.
      [None; None; None; None; Some (Some br); Some (Some pct); min; max]
  in
  table#set_row_data_some data row

let update_row_info (table : 'a Gadt_data_table.t) row pid (info : PID_info.t) =
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

let is_hex = Some true

class t ?(init : (int * PID_info.t) list ts option) (elt : Dom_html.element Js.t) () =
  object (self)
    val placeholder =
      match Element.query_selector elt Selector.placeholder with
      | Some x -> x
      | None -> Tyxml_js.To_dom.of_div @@ Markup_js.create_empty_placeholder ()

    val table : _ Gadt_data_table.t =
      Gadt_data_table.attach ~fmt:Markup_js.table_fmt
      @@ Element.query_selector_exn elt Selector.table

    val mutable data : Set.t =
      Set.of_list
        (match init with
        | None -> []
        | Some {data; _} -> data)

    inherit Widget.t elt () as super

    method! init () : unit =
      self#update_empty_state ();
      super#init ()

    method! destroy () : unit =
      table#destroy ();
      super#destroy ()

    method pids = Set.to_seq data

    method notify : event -> unit =
      function
      | `State x -> self#set_state x
      | `PIDs x -> self#set_pids x
      | `Bitrate x -> self#set_bitrate x

    method set_state state =
      let no_sync, no_response =
        match state with
        | `Fine -> false, false
        | `No_sync -> true, false
        | `Detect | `Init | `No_response -> false, true
      in
      Element.toggle_class_unit ~force:no_sync super#root CSS.no_sync;
      Element.toggle_class_unit ~force:no_response super#root CSS.no_response
    (** Updates widget state *)

    method set_bitrate : Bitrate.t option -> unit =
      function
      | None -> () (* FIXME do smth *)
      | Some {total; pids; _} ->
          List.iter
            (fun (pid, br) ->
              let row = self#find_row pid in
              Option.iter (update_row_bitrate table total br) row)
            pids
    (** Updates bitrate values *)

    method set_pids (pids : (int * PID_info.t) list ts) =
      (* Manage found, lost and updated items *)
      let old = data in
      let cur = Set.of_list pids.data in
      data <- Set.of_list pids.data;
      (* Handle lost PIDs *)
      Set.iter self#remove_pid @@ Set.diff old cur;
      (* Handle found PIDs *)
      Set.iter self#add_pid @@ Set.diff data old;
      (* Update existing PIDs *)
      Set.iter self#update_pid @@ Set.inter data old;
      self#update_empty_state ()

    method private update_pid (pid, info) =
      match self#find_row pid with
      | None -> ()
      | Some row -> update_row_info table row pid info

    method private remove_pid (pid, _) =
      match self#find_row pid with
      | None -> ()
      | Some row -> table#table##deleteRow row##.rowIndex

    method private add_pid (pid, info) =
      let flags = {has_pcr = info.has_pcr; scrambled = info.scrambled} in
      let (data : _ Markup_js.Fmt.data) =
        Markup_js.Fmt.[pid; info.typ; flags; info.service_name; None; None; None; None]
      in
      let row = table#insert_row (-1) data in
      Element.toggle_class_unit ~force:(not info.present) row CSS.row_lost

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

    method private update_empty_state () =
      if table#rows_collection##.length = 0
      then Dom.appendChild super#root placeholder
      else Element.remove placeholder
    (* method private set_hex (x : bool) : unit =
     *   let fmt = if x then hex_pid_fmt else dec_pid_fmt in
     *   let iter = function
     *     | Table.(pid :: _) -> pid#set_format fmt
     *   in
     *   List.iter (fun row -> iter row#cells) table#rows *)
  end

let attach ?init elt : t = new t ?init (elt : Dom_html.element Js.t) ()

let make ?classes ?attrs ?dense ?init () =
  Markup_js.create ?classes ?attrs ?dense ?init ()
  |> Tyxml_js.To_dom.of_div
  |> attach ?init
