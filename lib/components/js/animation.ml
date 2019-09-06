open Js_of_ocaml

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
