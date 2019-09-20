open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Board_niitv_tsan_types
include Board_niitv_tsan_widgets_tyxml.Pid_summary
module D = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

type event = [`PIDs of (int * PID.t) list ts]

module Attr = struct
  let data_pid = "data-pid"

  let data_type = "data-type"

  let data_has_pts = "data-has-pts"

  let data_has_pcr = "data-has-pcr"

  let data_scrambled = "data-scrambled"

  let data_lost = "data-lost"

  let data_service_id = "data-service-id"

  let data_service_name = "data-service-name"

  let set_pid_type typ elt =
    let typ =
      Yojson.Safe.to_string @@ Application_types.MPEG_TS.PID.Type.to_yojson typ
    in
    Element.set_attribute elt data_type typ

  let get_pid_type elt =
    match Element.get_attribute elt data_type with
    | None -> raise Not_found
    | Some x -> (
        let result =
          Application_types.MPEG_TS.PID.Type.of_yojson @@ Yojson.Safe.from_string x
        in
        match result with
        | Error e -> failwith e
        | Ok x -> x)

  let pid_info_of_element elt : PID.t =
    { typ = get_pid_type elt
    ; has_pts = Element.has_attribute elt data_has_pts
    ; has_pcr = Element.has_attribute elt data_has_pcr
    ; scrambled = Element.has_attribute elt data_scrambled
    ; present = not @@ Element.has_attribute elt data_lost
    ; service_name = Element.get_attribute elt data_service_name
    ; service_id =
        Option.join
        @@ Option.map int_of_string_opt
        @@ Element.get_attribute elt data_service_id }

  let set_pid_info (info : PID.t) elt =
    let set_bool elt attr x =
      if x then Element.set_attribute elt attr "" else Element.remove_attribute elt attr
    in
    let set_option elt attr f = function
      | None -> Element.remove_attribute elt attr
      | Some x -> Element.set_attribute elt attr (f x)
    in
    set_pid_type info.typ elt;
    set_bool elt data_has_pts info.has_pts;
    set_bool elt data_has_pcr info.has_pcr;
    set_bool elt data_scrambled info.scrambled;
    set_bool elt data_lost (not info.present);
    set_option elt data_service_id string_of_int info.service_id;
    set_option elt data_service_name (fun x -> x) info.service_name
end

module Selector = struct
  let pid = Printf.sprintf ".%s" CSS.pid

  let pids = Printf.sprintf ".%s" CSS.pids

  let pid' pid = Printf.sprintf ".%s[%s=\"%d\"]" CSS.pid Attr.data_pid pid
end

module Set = Set.Make (struct
  type t = int * PID.t

  let compare (a : t) (b : t) : int = Int.compare (fst a) (fst b)
end)

class t (elt : Dom_html.element Js.t) =
  object (self)
    inherit Widget.t elt () as super

    val pids = Element.query_selector_exn elt Selector.pids

    method pids : (int * PID.t) list =
      List.map (fun x ->
          let text =
            Js.Opt.get x##.textContent (fun () ->
                let msg = CSS.root ^ ": text content is not set for PID element" in
                failwith msg)
          in
          int_of_string @@ Js.to_string text, Attr.pid_info_of_element x)
      @@ Element.query_selector_all super#root Selector.pid

    method notify : event -> unit =
      function
      | `PIDs x -> self#set_pids x

    (* Private methods *)
    method private set_pids ({data; _} : (int * PID.t) list ts) =
      let cur = Set.of_list data in
      let old = Set.of_list self#pids in
      (* Handle lost PIDs *)
      Set.iter self#remove_pid @@ Set.diff old cur;
      (* Handle found PIDs *)
      Set.iter self#add_pid @@ Set.diff cur old;
      (* Update existing PIDs *)
      Set.iter self#update_pid @@ Set.inter old cur

    method update_pid (pid, info) =
      match Element.query_selector super#root (Selector.pid' pid) with
      | None -> ()
      | Some x -> Attr.set_pid_info info x

    method private remove_pid (pid, _) =
      match Element.query_selector super#root (Selector.pid' pid) with
      | None -> ()
      | Some x -> Element.remove x

    method private add_pid pid =
      let elt = Tyxml_js.To_dom.of_element @@ D.create_pid ~pid () in
      Dom.appendChild pids elt
  end

let attach (elt : #Dom_html.element Js.t) : t = new t (elt :> Dom_html.element Js.t)

let make ?classes ?a ?hex ?pids () : t =
  D.create ?classes ?a ?hex ?pids () |> Tyxml_js.To_dom.of_div |> attach