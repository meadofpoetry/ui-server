open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Board_niitv_tsan_types
include Board_niitv_tsan_widgets_tyxml.Si_psi_overview
module D = Make (Impl.Xml) (Impl.Svg) (Impl.Html)
module R = Make (Impl.R.Xml) (Impl.R.Svg) (Impl.R.Html)

module Set = Set.Make (struct
  type t = SI_PSI_table.id * SI_PSI_table.t

  let compare (a : t) (b : t) : int = SI_PSI_table.compare_id (fst a) (fst b)
end)

let get_attribute elt attr = Element.get_attribute (Tyxml_js.To_dom.of_element elt) attr

class t ?(init : (SI_PSI_table.id * SI_PSI_table.t) list ts option) elt () =
  object
    val mutable data : Set.t =
      Set.of_list
        (match init with
        | None -> []
        | Some {data; _} -> data)

    inherit [SI_PSI_table.id] Table_overview.with_details elt ()

    method private get_row_title _ = ""

    method private handle_row_action _ = Lwt.return_unit
  end

let attach elt : t = new t (elt :> Dom_html.element Js.t) ()
