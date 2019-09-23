open Js_of_ocaml
open Js_of_ocaml_tyxml
open Application_types
open Board_niitv_tsan_types
open Components
include Board_niitv_tsan_widgets_tyxml.Pid_overview
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)
module R = Make (Tyxml_js.R.Xml) (Tyxml_js.R.Svg) (Tyxml_js.R.Html)

type event = [`State of [Topology.state | `No_sync]]

class t (elt : Dom_html.element Js.t) () =
  object
    inherit
      [int] Table_overview.t ~create_table_format:D.create_table_format elt () as super

    method notify : event -> unit =
      function
      | `State x -> super#set_state x

    method set_hex (hex : bool) : unit =
      let pid_fmt = D.pid_fmt ~hex in
      let (format : _ D.Fmt.data_format) =
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

    method private find_row (pid : int) =
      let find row =
        let pid' =
          Gadt_data_table.Fmt_d.(
            match table#get_row_data_lazy row with
            | pid :: _ -> pid ())
        in
        pid = pid'
      in
      List.find_opt find table#rows
  end

let attach elt : t = new t (elt : Dom_html.element Js.t) ()

let make ?a ?dense ?init ?hex ~control () =
  D.create ?a ?dense ?init ?hex ~control () |> Tyxml_js.To_dom.of_div |> attach
