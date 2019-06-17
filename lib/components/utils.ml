open Js_of_ocaml

let (=.) : float -> float -> bool = Pervasives.(=)
let (<>.) : float -> float -> bool = Pervasives.(<>)
let (<.) : float -> float -> bool = Pervasives.(<)
let (>.) : float -> float -> bool = Pervasives.(>)
let (<=.) : float -> float -> bool = Pervasives.(<=)
let (>=.) : float -> float -> bool = Pervasives.(>=)

let ( % ) f g x = f (g x)

module Bool = struct
  type t = bool

  let equal (a : t) (b : t) : t = Pervasives.( = ) a b
end

module Float = struct
  include Float

  module Infix = struct
    let (=) : t -> t -> bool = Pervasives.(=)
    let (<>) : t -> t -> bool = Pervasives.(<>)
    let (<) : t -> t -> bool = Pervasives.(<)
    let (>) : t -> t -> bool = Pervasives.(>)
    let (<=) : t -> t -> bool = Pervasives.(<=)
    let (>=) : t -> t -> bool = Pervasives.(>=)
    let (~-) : t -> t = Pervasives.(~-.)
    let (+) : t -> t -> t = Pervasives.(+.)
    let (-) : t -> t -> t = Pervasives.(-.)
    let ( * ) : t -> t -> t = Pervasives.( *. )
    let (/) : t -> t -> t = Pervasives.(/.)
  end
  include Infix

  let min (a : t) (b : t) : t = Pervasives.min a b
  let max (a : t) (b : t) : t = Pervasives.max a b

  let round (x : float) : float =
    floor (x +. 0.5)
end

module List = struct
  include List

  let rec equal ~eq l1 l2 = match l1, l2 with
    | [], [] -> true
    | [], _ | _, [] -> false
    | x1 :: l1', x2 :: l2' -> eq x1 x2 && equal ~eq l1' l2'

  let mem ~eq x l =
    let rec aux eq x = function
      | [] -> false
      | y :: l' -> eq x y || aux eq x l'
    in aux eq x l

  let add_nodup ~eq x l =
    if mem ~eq x l then l else x :: l

  let find_mapi f l =
    let rec aux f i = function
      | [] -> None
      | x :: l' ->
         match f i x with
         | Some _ as res -> res
         | None -> aux f (i + 1) l'
    in aux f 0 l

  let find_map f l = find_mapi (fun _ -> f) l

  let hd_opt = function
    | x :: _ -> Some x | [] -> None

  let remove ~eq x l =
    let rec aux eq x acc = function
      | [] -> rev acc
      | y :: tl when eq x y -> aux eq x acc tl
      | y :: tl -> aux eq x (y :: acc) tl in
    aux eq x [] l

  let cons_maybe (x : 'a option) (l : 'a list) : 'a list =
    match x with
    | None -> l
    | Some x -> x :: l

  let filter_map f l =
    let rec aux acc = function
      | [] -> List.rev acc
      | x :: l' ->
         let acc' = match f x with | None -> acc | Some y -> y :: acc in
         aux acc' l'
    in aux [] l

  let rec fold_while f acc = function
    | [] -> acc
    | e :: l -> let acc, cont = f acc e in
              match cont with
              | `Stop -> acc
              | `Continue -> fold_while f acc l

  module Assoc = struct
    type ('a, 'b) t = ('a * 'b) list

    let rec search_exn eq l x = match l with
      | [] -> raise Not_found
      | (y, z) :: l' ->
         if eq x y then z else search_exn eq l' x

    let get_exn ~eq x l = search_exn eq l x

    let get ~eq x l =
      try Some (search_exn eq l x)
      with Not_found -> None

    (* search for a binding for [x] in [l], and calls [f x (Some v) rest]
     or [f x None rest] depending on whether it finds the binding.
     [rest] is the list of the other bindings *)
    let rec search_set eq acc l x ~f = match l with
      | [] -> f x None acc
      | (x',y')::l' ->
         if eq x x'
         then f x (Some y') (List.rev_append acc l')
         else search_set eq ((x',y')::acc) l' x ~f

    let set ~eq x y l =
      search_set eq [] l x
        ~f:(fun x _ l -> (x,y)::l)

    let mem ~eq x l =
      try ignore (search_exn eq l x); true
      with Not_found -> false

    let update ~eq f x l =
      search_set eq [] l x
        ~f:(fun x opt_y rest ->
          match f opt_y with
          | None -> rest (* drop *)
          | Some y' -> (x, y') :: rest)

    let remove ~eq x l =
      search_set eq [] l x
        ~f:(fun _ opt_y rest -> match opt_y with
                                | None -> l  (* keep as is *)
                                | Some _ -> rest)
  end
end

module Option = struct
  let equal ~(eq : 'a -> 'a -> bool) (a : 'a option) (b : 'a option) : bool =
    match a, b with
    | None, None -> true
    | Some a, Some b -> eq a b
    | _, _ -> false

  let get = function Some x -> x | None -> invalid_arg "value is None"

  let iter (f : 'a -> unit) : 'a option -> unit = function
    | None -> ()
    | Some x -> f x

  let map (f : 'a -> 'b) : 'a option -> 'b option = function
    | None -> None
    | Some x -> Some (f x)

  let bind f = function None -> None | Some x -> f x

  let is_some : 'a option -> bool = function
    | None -> false
    | Some _ -> true

  let is_none : 'a option -> bool = function
    | None -> true
    | Some _ -> false
end

module String = struct
  include String

  let suffix ~suf s =
    let len = String.length suf in
    if len > String.length s then false
    else (
      let off = String.length s - len in
      let rec check i =
        if i = len then true
        else if Pervasives.(<>) (String.unsafe_get s (off + i)) (String.unsafe_get suf i)
        then false
        else check (i + 1)
      in
      check 0)

  let chop_suffix ~suf s =
    if suffix ~suf s
    then Some (String.sub s 0 (String.length s - String.length suf))
    else None

  let prefix ~pre s =
    let len = String.length pre in
    if len > String.length s then false
    else (
      let rec check i =
        if i = len then true
        else if Pervasives.(<>)
                  (String.unsafe_get s i)
                  (String.unsafe_get pre i) then false
        else check (i + 1)
      in
      check 0)

  let chop_prefix ~pre s =
    if prefix ~pre s
    then Some (String.sub s (String.length pre) (String.length s - String.length pre))
    else None
end

let prevent_scroll = ref false

let clamp ?(min = 0.) ?(max = 100.) (v : float) : float =
  Float.min (Float.max v min) max

let is_in_viewport ?(vertical = true) ?(horizontal = true)
      (e : Dom_html.element Js.t) : bool =
  let height =
    Js.Optdef.get Dom_html.window##.innerHeight
      (fun () -> Dom_html.document##.documentElement##.clientHeight) in
  let width =
    Js.Optdef.get Dom_html.window##.innerWidth
      (fun () -> Dom_html.document##.documentElement##.clientWidth) in
  let rect = e##getBoundingClientRect in
  let vertical =
    not vertical
    || (rect##.top >. 0. && rect##.bottom <=. (float_of_int height)) in
  let horizontal =
    not horizontal
    || (rect##.left >. 0. && rect##.right <=. (float_of_int width)) in
  vertical && horizontal

(** Tail-recursive append that does not raise stack overflow on lagre lists *)
let append (a : 'a list) (b : 'a list) : 'a list =
  List.rev_append (List.rev a) b

let ( @ ) = append

let time (name : string) : unit =
  Js.Unsafe.global##.console##time (Js.string name)

let time_end (name : string) : unit =
  Js.Unsafe.global##.console##timeEnd (Js.string name)

let px : int -> string = Printf.sprintf "%dpx"

let px_js (v : int) : Js.js_string Js.t =
  Js.string @@ px v

let translate = Printf.sprintf "translate(%dpx, %dpx)"

let ( // ) x y =
  Float.round @@ (float_of_int x) /. (float_of_int y)
  |> int_of_float

let rec gcd a b =
  if a <> 0 && b <> 0
  then
    let a, b =
      if a > b then a mod b, b
      else a, b mod a in gcd a b
  else a + b

let resolution_to_aspect (w,h) =
  let d = gcd w h in w / d, h / d

let sum_scroll_offsets (e : Dom_html.element Js.t) =
  let rec aux cur acc_left acc_top =
    match Js.Opt.to_option cur with
    | None -> acc_left, acc_top
    | Some cur ->
       begin match Js.to_string cur##.nodeName with
       | "BODY" -> acc_left,acc_top
       | _ ->
          aux cur##.parentNode
            (acc_left + (Js.Unsafe.coerce cur)##.scrollLeft)
            (acc_top + (Js.Unsafe.coerce cur)##.scrollTop)
       end
  in
  aux e##.parentNode 0 0

let find_element_by_class_exn (elt : #Dom_html.element Js.t)
      (_class : string) : #Dom_html.element Js.t =
  elt##querySelector (Js.string ("." ^ _class))
  |> (fun x ->
    Js.Opt.get x (fun () ->
        let s = Printf.sprintf "No '%s' element found" _class in
        failwith s))
  |> Js.Unsafe.coerce

module Animation = struct

  let request () : float Lwt.t =
    let t, w = Lwt.task () in
    let id =
      Dom_html.window##requestAnimationFrame
        (Js.wrap_callback (Lwt.wakeup w)) in
    Lwt.on_cancel t (fun () -> Dom_html.window##cancelAnimationFrame id);
    t

  type vendor_property_map =
    { no_prefix : string
    ; webkit_prefix : string
    ; style_property : string
    }

  type map = (string * vendor_property_map) list

  let make_vendor_property_map ?(style_property = "")
        ~no_prefix ~webkit_prefix () =
    { no_prefix; webkit_prefix; style_property }

  let (event_type_map : map) =
    [ "animationstart",
      make_vendor_property_map
        ~no_prefix:"animationstart"
        ~webkit_prefix:"webkitAnimationStart"
        ~style_property:"animation"
        ()
    ; "animationend",
      make_vendor_property_map
        ~no_prefix:"animationend"
        ~webkit_prefix:"webkitAnimationEnd"
        ~style_property:"animation"
        ()
    ; "animationiteration",
      make_vendor_property_map
        ~no_prefix:"animationiteration"
        ~webkit_prefix:"webkitAnimationIteration"
        ~style_property:"animation"
        ()
    ; "tranisitionend",
      make_vendor_property_map
        ~no_prefix:"transitionend"
        ~webkit_prefix:"webkitTransitionEnd"
        ~style_property:"transition"
        ()
    ]

  let (css_property_map : map) =
    [ "animation",
      make_vendor_property_map
      ~no_prefix:"animation"
      ~webkit_prefix:"-webkit-animation"
      ()
    ; "transform",
      make_vendor_property_map
        ~no_prefix:"transform"
        ~webkit_prefix:"-webkit-transform"
        ()
    ; "transition",
      make_vendor_property_map
        ~no_prefix:"transition"
        ~webkit_prefix:"-webkit-transition"
        ()
    ]

  let has_proper_shape ~(window : 'a Js.t) : bool =
    Js.Optdef.test (Js.Unsafe.coerce window)##.document
    && (
      let typ =
        Js.to_string @@ Js.typeof
        @@ (Js.Unsafe.coerce window)##.document##.createElement in
      String.equal "function" typ)

  let get_event_name ~(map_type : [`CSS | `JS])
        (map : vendor_property_map)
        (elt : Dom_html.element Js.t) : string =
    let to_test = match map_type with
      | `CSS -> map.no_prefix
      | `JS -> map.style_property in
    if Js.Optdef.test
       @@ Js.Unsafe.get elt##.style (Js.string to_test)
    then map.no_prefix else map.webkit_prefix

  let get_animation_name ~(window : 'a Js.t) (event_type : string) : string =
    let eq = String.equal in
    match has_proper_shape ~window,
          List.Assoc.get ~eq event_type event_type_map,
          List.Assoc.get ~eq event_type css_property_map with
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
