open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Board_niitv_tsan_types
include Board_niitv_tsan_widgets_tyxml.Pid_summary
module Markup_js = Make (Tyxml_js.Xml) (Tyxml_js.Svg) (Tyxml_js.Html)

let name = "PID summary"

type event = [`PIDs of (int * PID_info.t) list ts]

module Attr = struct
  let pid_type = "data-type"

  let has_pts = "data-has-pts"

  let has_pcr = "data-has-pcr"

  let scrambled = "data-scrambled"

  let lost = "data-lost"

  let service_id = "data-service-id"

  let service_name = "data-service-name"

  let set_pid_type typ elt =
    let typ =
      Yojson.Safe.to_string @@ Application_types.MPEG_TS.PID.Type.to_yojson typ
    in
    Element.set_attribute elt pid_type typ

  let get_pid_type elt =
    match Element.get_attribute elt pid_type with
    | None -> raise Not_found
    | Some x -> (
        let result =
          Application_types.MPEG_TS.PID.Type.of_yojson @@ Yojson.Safe.from_string x
        in
        match result with
        | Error e -> failwith e
        | Ok x -> x)

  let pid_info_of_element elt : PID_info.t =
    { typ = get_pid_type elt
    ; has_pts = Element.has_attribute elt has_pts
    ; has_pcr = Element.has_attribute elt has_pcr
    ; scrambled = Element.has_attribute elt scrambled
    ; present = not @@ Element.has_attribute elt lost
    ; service_name = Element.get_attribute elt service_name
    ; service_id =
        Option.join
        @@ Option.map int_of_string_opt
        @@ Element.get_attribute elt service_id }

  let set_pid_info (info : PID_info.t) elt =
    let set_bool elt attr x =
      if x then Element.set_attribute elt attr "" else Element.remove_attribute elt attr
    in
    let set_option elt attr f = function
      | None -> Element.remove_attribute elt attr
      | Some x -> Element.set_attribute elt attr (f x)
    in
    set_pid_type info.typ elt;
    set_bool elt has_pts info.has_pts;
    set_bool elt has_pcr info.has_pcr;
    set_bool elt scrambled info.scrambled;
    set_bool elt lost (not info.present);
    set_option elt service_id string_of_int info.service_id;
    set_option elt service_name (fun x -> x) info.service_name
end

module Selector = struct
  let pid = Printf.sprintf ".%s" CSS.pid

  let pids = Printf.sprintf ".%s" CSS.pids

  let pid' pid = Printf.sprintf ".%s[data-pid=\"%d\"]" CSS.pid pid
end

module Set = Set.Make (struct
  type t = int * PID_info.t

  let compare (a : t) (b : t) : int = Int.compare (fst a) (fst b)
end)

class t (elt : Dom_html.element Js.t) =
  object (self)
    inherit Widget.t elt () as super

    val pids = Element.query_selector_exn elt Selector.pids

    method pids : (int * PID_info.t) list =
      List.map (fun x ->
          let text =
            Js.Opt.get x##.textContent (fun () ->
                let msg = name ^ ": text content is not set for PID element" in
                failwith msg)
          in
          int_of_string @@ Js.to_string text, Attr.pid_info_of_element x)
      @@ Element.query_selector_all super#root Selector.pid

    method notify : event -> unit =
      function
      | `PIDs x -> self#set_pids x

    (* Private methods *)
    method private set_pids ({data; _} : (int * PID_info.t) list ts) =
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
      let elt = Tyxml_js.To_dom.of_element @@ Markup_js.create_pid pid in
      Dom.appendChild pids elt
  end

let make () : t =
  let elt = Tyxml_js.To_dom.of_element @@ Markup_js.create () in
  new t elt

let attach (elt : #Dom_html.element Js.t) : t = new t (elt :> Dom_html.element Js.t)
