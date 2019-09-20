open Components_tyxml
open Board_niitv_tsan_types

module CSS = struct
  let root = Util.CSS.root ^ "-service-general-info"

  let item = BEM.add_element root "item"
end

let not_available = "n/a"

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml)
    (Html : Html_sigs.T with module Xml := Xml and module Svg := Svg) =
struct
  open Xml.W
  open Html

  open Item_list.Make (Xml) (Svg) (Html)

  let ( @:: ) x l = cons (return x) l

  let create_item_meta ?(a = []) ~text () =
    span
      ~a:(a_class (return [Item_list.CSS.item_meta]) :: a)
      (singleton (return (txt text)))

  let create_item ?a ?meta ~primary_text () =
    let classes = return [CSS.item] in
    let meta =
      match meta with
      | None -> None
      | Some s -> Some (return (create_item_meta ~text:s ()))
    in
    list_item ~classes ?a ~primary_text ?meta ()

  let create
      ?(a = [])
      ?children
      ?(bitrate = return None)
      ?(max_bitrate = return None)
      ?(min_bitrate = return None)
      ?(info : (int * Service.t) option wrap = return None)
      () =
    let classes = return [CSS.root] in
    let elements =
      fmap
        (function
          | None | Some (_, {Service.elements = []; _}) -> ""
          | Some (_, info) ->
              let elements = Util.service_pids info in
              String.concat "," @@ List.map string_of_int elements)
        info
    in
    let children =
      match children with
      | Some x -> x
      | None ->
          let meta f info =
            fmap
              (function
                | None -> not_available
                | Some x -> f x)
              info
          in
          create_item
            ~a:[a_user_data "type" (return "service-id")]
            ~primary_text:(`Text (return "Service ID"))
            ~meta:(meta (fun (x, _) -> string_of_int x) info)
            ()
          @:: create_item
                ~a:[a_user_data "type" (return "pmt-pid")]
                ~primary_text:(`Text (return "PMT PID"))
                ~meta:(meta (fun (_, (x : Service.t)) -> string_of_int x.pmt_pid) info)
                ()
          @:: create_item
                ~a:[a_user_data "type" (return "pcr-pid")]
                ~primary_text:(`Text (return "PCR PID"))
                ~meta:(meta (fun (_, (x : Service.t)) -> string_of_int x.pcr_pid) info)
                ()
          @:: create_item
                ~a:[a_user_data "type" (return "bitratenow")]
                ~primary_text:(`Text (return "Битрейт"))
                ~meta:(meta (fun x -> string_of_int x) bitrate)
                ()
          @:: create_item
                ~a:[a_user_data "type" (return "bitratemin")]
                ~primary_text:(`Text (return "Min"))
                ~meta:(meta (fun x -> string_of_int x) min_bitrate)
                ()
          @:: create_item
                ~a:[a_user_data "type" (return "bitratemax")]
                ~primary_text:(`Text (return "Max"))
                ~meta:(meta (fun x -> string_of_int x) max_bitrate)
                ()
          @:: nil ()
    in
    list
      ~classes
      ~a:(a_user_data "elements" elements :: a)
      ~dense:true
      ~non_interactive:true
      ~children
      ()
end

module F = Make (Tyxml.Xml) (Tyxml.Svg) (Tyxml.Html)
