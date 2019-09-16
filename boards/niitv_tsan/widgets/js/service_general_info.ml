open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Board_niitv_tsan_types
include Board_niitv_tsan_widgets_tyxml.Service_general_info
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

type event =
  [ `Service of (int * Service.t) option
  | `Bitrate of int Bitrate.t option ]

module Attr = struct
  type typ =
    [ `Service_id
    | `PMT_PID
    | `PCR_PID
    | `Bitrate_now
    | `Bitrate_min
    | `Bitrate_max ]

  let data_elements = "data-elements"

  let data_type = "data-type"

  let get_type (elt : #Dom_html.element Js.t) : typ option =
    Js.Opt.case
      (elt##getAttribute (Js.string data_type))
      (fun () -> None)
      (fun s ->
        match Js.to_string s with
        | "service-id" -> Some `Service_id
        | "pmt-pid" -> Some `PMT_PID
        | "pcr-pid" -> Some `PCR_PID
        | "bitratenow" -> Some `Bitrate_now
        | "bitratemin" -> Some `Bitrate_min
        | "bitratemax" -> Some `Bitrate_max
        | _ -> None)
end

module Selector = struct
  let item = Printf.sprintf ".%s[%s]" CSS.item Attr.data_type

  let meta = "." ^ Item_list.CSS.item_meta
end

let set_meta_text meta text = meta##.textContent := Js.some @@ Js.string text

let set_item_not_available item =
  Js.Opt.iter
    (item##querySelector (Js.string Selector.meta))
    (fun meta -> set_meta_text meta not_available)

class t (elt : Dom_html.element Js.t) () =
  object (self)
    inherit Widget.t elt () as super

    val mutable elements =
      match Element.get_attribute elt Attr.data_elements with
      | None -> []
      | Some s -> (
        try
          let pids = String.split_on_char ',' s in
          List.map (fun x -> int_of_string (String.trim x)) pids
        with _ -> [])

    method elements : int list = elements

    method hex : bool = false

    method notify : event -> unit =
      function
      | `Service info -> self#set_service_info info
      | `Bitrate rate -> self#set_bitrate rate

    method set_bitrate bitrate =
      let items = Element.query_selector_all super#root Selector.item in
      match bitrate with
      | None ->
          List.iter
            (fun item ->
              match Attr.get_type item with
              | Some (`Bitrate_now | `Bitrate_min | `Bitrate_max) ->
                  set_item_not_available item
              | _ -> ())
            items
      | Some bitrate ->
          List.iter
            (fun item ->
              match Attr.get_type item with
              | Some (`Bitrate_now | `Bitrate_min | `Bitrate_max) ->
                  let meta = self#get_item_meta item in
                  let rate = Util.total_bitrate_for_pids bitrate elements in
                  self#set_meta_bitrate meta rate
              | _ -> ())
            items

    method set_service_info info =
      let items = Element.query_selector_all super#root Selector.item in
      match info with
      | None -> List.iter set_item_not_available items
      | Some info ->
          elements <- Util.service_pids (snd info);
          List.iter
            (fun item ->
              match Attr.get_type item with
              | None -> ()
              | Some typ -> self#set_item_value item typ info)
            items

    method private set_meta_bitrate meta rate =
      set_meta_text
        meta
        (Printf.sprintf "%f Мбит/с" (float_of_int rate /. 1_000_000.))

    method private set_meta_value meta ((id, info) : int * Service.t) =
      function
      | `Service_id -> set_meta_text meta (Util.pid_to_string ~hex:self#hex id)
      | `PMT_PID -> set_meta_text meta (Util.pid_to_string ~hex:self#hex info.pmt_pid)
      | `PCR_PID -> set_meta_text meta (Util.pid_to_string ~hex:self#hex info.pcr_pid)
      | `Bitrate_now | `Bitrate_min | `Bitrate_max -> ()

    method private set_item_value item typ info =
      let meta = self#get_item_meta item in
      self#set_meta_value meta info typ

    method private get_item_meta item =
      match Element.query_selector item Selector.meta with
      | Some x -> x
      | None ->
          let meta =
            Tyxml_js.To_dom.of_element @@ Markup_js.create_item_meta ~text:"" ()
          in
          Dom.appendChild item meta;
          meta
  end

let attach elt = new t (elt :> Dom_html.element Js.t) ()

let make ?classes ?attrs ?info ?children () =
  Markup_js.create ?classes ?attrs ?info ?children () |> Tyxml_js.To_dom.of_ul |> attach
