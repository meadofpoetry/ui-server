open Js_of_ocaml

let ( % ) f g x = f (g x)

let prevent_scroll = ref false

let clamp ?(min = 0.) ?(max = 100.) (v : float) : float = Float.min (Float.max v min) max

let is_in_viewport ?(vertical = true) ?(horizontal = true) (e : Dom_html.element Js.t) :
    bool =
  let height =
    Js.Optdef.get Dom_html.window##.innerHeight (fun () ->
        Dom_html.document##.documentElement##.clientHeight)
  in
  let width =
    Js.Optdef.get Dom_html.window##.innerWidth (fun () ->
        Dom_html.document##.documentElement##.clientWidth)
  in
  let rect = e##getBoundingClientRect in
  let vertical =
    (not vertical) || (rect##.top > 0. && rect##.bottom <= float_of_int height)
  in
  let horizontal =
    (not horizontal) || (rect##.left > 0. && rect##.right <= float_of_int width)
  in
  vertical && horizontal

(** Tail-recursive append that does not raise stack overflow on lagre lists *)
let append (a : 'a list) (b : 'a list) : 'a list = List.rev_append (List.rev a) b

let ( @ ) = append

let px : int -> string = Printf.sprintf "%dpx"

let px_js (v : int) : Js.js_string Js.t = Js.string @@ px v

let sum_scroll_offsets (e : Dom_html.element Js.t) =
  let rec aux cur acc_left acc_top =
    match Js.Opt.to_option cur with
    | None -> acc_left, acc_top
    | Some cur -> (
      match Js.to_string cur##.nodeName with
      | "BODY" -> acc_left, acc_top
      | _ ->
          aux
            cur##.parentNode
            (acc_left + (Js.Unsafe.coerce cur)##.scrollLeft)
            (acc_top + (Js.Unsafe.coerce cur)##.scrollTop))
  in
  aux e##.parentNode 0 0

module Animation = struct
  type vendor_property_map =
    { no_prefix : string
    ; webkit_prefix : string
    ; style_property : string }

  type map = (string * vendor_property_map) list

  let make_vendor_property_map ?(style_property = "") ~no_prefix ~webkit_prefix () =
    {no_prefix; webkit_prefix; style_property}

  let (event_type_map : map) =
    [ ( "animationstart"
      , make_vendor_property_map
          ~no_prefix:"animationstart"
          ~webkit_prefix:"webkitAnimationStart"
          ~style_property:"animation"
          () )
    ; ( "animationend"
      , make_vendor_property_map
          ~no_prefix:"animationend"
          ~webkit_prefix:"webkitAnimationEnd"
          ~style_property:"animation"
          () )
    ; ( "animationiteration"
      , make_vendor_property_map
          ~no_prefix:"animationiteration"
          ~webkit_prefix:"webkitAnimationIteration"
          ~style_property:"animation"
          () )
    ; ( "tranisitionend"
      , make_vendor_property_map
          ~no_prefix:"transitionend"
          ~webkit_prefix:"webkitTransitionEnd"
          ~style_property:"transition"
          () ) ]

  let (css_property_map : map) =
    [ ( "animation"
      , make_vendor_property_map
          ~no_prefix:"animation"
          ~webkit_prefix:"-webkit-animation"
          () )
    ; ( "transform"
      , make_vendor_property_map
          ~no_prefix:"transform"
          ~webkit_prefix:"-webkit-transform"
          () )
    ; ( "transition"
      , make_vendor_property_map
          ~no_prefix:"transition"
          ~webkit_prefix:"-webkit-transition"
          () ) ]

  let has_proper_shape ~(window : 'a Js.t) : bool =
    Js.Optdef.test (Js.Unsafe.coerce window)##.document
    &&
    let typ =
      Js.to_string @@ Js.typeof @@ (Js.Unsafe.coerce window)##.document##.createElement
    in
    String.equal "function" typ

  let get_event_name
      ~(map_type : [`CSS | `JS])
      (map : vendor_property_map)
      (elt : Dom_html.element Js.t) : string =
    let to_test =
      match map_type with
      | `CSS -> map.no_prefix
      | `JS -> map.style_property
    in
    if Js.Optdef.test @@ Js.Unsafe.get elt##.style (Js.string to_test)
    then map.no_prefix
    else map.webkit_prefix

  let get_animation_name ~(window : 'a Js.t) (event_type : string) : string =
    match
      ( has_proper_shape ~window
      , List.assoc_opt event_type event_type_map
      , List.assoc_opt event_type css_property_map )
    with
    | false, _, _ | _, None, None -> event_type
    | _, Some t, _ ->
        let (wnd : Dom_html.window Js.t) = Js.Unsafe.coerce window in
        let elt = wnd##.document##createElement (Js.string "div") in
        get_event_name ~map_type:`JS t elt
    | _, _, Some t ->
        let (wnd : Dom_html.window Js.t) = Js.Unsafe.coerce window in
        let elt = wnd##.document##createElement (Js.string "div") in
        get_event_name ~map_type:`CSS t elt

  let get_correct_event_name ~(window : 'a Js.t) (event_type : string) =
    get_animation_name ~window event_type

  let get_correct_property_name ~(window : 'a Js.t) (event_type : string) =
    get_animation_name ~window event_type
end

module List = struct
  let rec equal ~eq l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | [], _ | _, [] -> false
    | x1 :: l1', x2 :: l2' -> eq x1 x2 && equal ~eq l1' l2'

  let add_nodup ~eq x l = if List.exists (eq x) l then l else x :: l

  let find_mapi f l =
    let rec aux f i = function
      | [] -> None
      | x :: l' -> (
        match f i x with
        | Some _ as res -> res
        | None -> aux f (i + 1) l')
    in
    aux f 0 l

  let find_map f l = find_mapi (fun _ -> f) l

  let cons_maybe (x : 'a option) (l : 'a list) : 'a list =
    match x with
    | None -> l
    | Some x -> x :: l

  module Assoc = struct
    type ('a, 'b) t = ('a * 'b) list

    (* search for a binding for [x] in [l], and calls [f x (Some v) rest]
     or [f x None rest] depending on whether it finds the binding.
     [rest] is the list of the other bindings *)
    let rec search_set eq acc l x ~f =
      match l with
      | [] -> f x None acc
      | (x', y') :: l' ->
          if eq x x'
          then f x (Some y') (List.rev_append acc l')
          else search_set eq ((x', y') :: acc) l' x ~f

    let update ~eq f x l =
      search_set eq [] l x ~f:(fun x opt_y rest ->
          match f opt_y with
          | None -> rest (* drop *)
          | Some y' -> (x, y') :: rest)
  end
end

module String = struct
  let suffix ~suf s =
    let len = String.length suf in
    if len > String.length s
    then false
    else
      let off = String.length s - len in
      let rec check i =
        if i = len
        then true
        else if String.unsafe_get s (off + i) <> String.unsafe_get suf i
        then false
        else check (i + 1)
      in
      check 0

  let chop_suffix ~suf s =
    if suffix ~suf s
    then Some (String.sub s 0 (String.length s - String.length suf))
    else None

  let prefix ~pre s =
    let len = String.length pre in
    if len > String.length s
    then false
    else
      let rec check i =
        if i = len
        then true
        else if String.unsafe_get s i <> String.unsafe_get pre i
        then false
        else check (i + 1)
      in
      check 0

  let chop_prefix ~pre s =
    if prefix ~pre s
    then Some (String.sub s (String.length pre) (String.length s - String.length pre))
    else None
end
