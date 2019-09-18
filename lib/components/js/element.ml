open Js_of_ocaml

let event_detail (e : 'a #Dom_html.customEvent Js.t) =
  Js.Opt.get e##.detail (fun () -> failwith "No detail")

type t = Dom_html.element Js.t

let equal (a : #Dom_html.element Js.t as 'a) (b : 'a) : bool = a == b

let coerce (elt : #Dom_html.element Js.t) : t = (elt :> t)

let contains (container : #Dom.node Js.t) (elt : #Dom.node Js.t) : bool =
  Js.to_bool @@ (Js.Unsafe.coerce container)##contains elt

let classes (elt : #Dom_html.element Js.t) : string list =
  String.split_on_char ' ' @@ Js.to_string @@ elt##.className

let children (elt : #Dom_html.element Js.t) : t list =
  List.filter_map (fun (x : Dom.node Js.t) ->
      match x##.nodeType with
      | ELEMENT -> Some (Js.Unsafe.coerce x)
      | _ -> None)
  @@ Dom.list_of_nodeList elt##.childNodes

let append_child (elt : #Dom.node Js.t) (child : #Dom.node Js.t) : unit =
  Dom.appendChild elt child

let insert_child_at_index
    (parent : #Dom.node Js.t)
    (index : int)
    (child : #Dom.node Js.t) : unit =
  let sibling = (Js.Unsafe.coerce parent)##.children##item index in
  Dom.insertBefore parent child sibling

let remove_child_safe (elt : #Dom.node Js.t) (child : #Dom.node Js.t) : unit =
  try Dom.removeChild elt child with _ -> ()

let remove_children (elt : #Dom_html.element Js.t) =
  Dom.list_of_nodeList @@ elt##.childNodes |> List.iter (fun x -> Dom.removeChild elt x)

let add_class (elt : #Dom_html.element Js.t) (_class : string) : unit =
  elt##.classList##add (Js.string _class)

let remove_class (elt : #Dom_html.element Js.t) (_class : string) : unit =
  elt##.classList##remove (Js.string _class)

let toggle_class ?(force : bool option) (elt : #Dom_html.element Js.t) (_class : string)
    : bool =
  match force with
  | None -> Js.to_bool @@ elt##.classList##toggle (Js.string _class)
  | Some true ->
      add_class elt _class;
      true
  | Some false ->
      remove_class elt _class;
      false

let toggle_class_unit ?force elt _class =
  let (_ : bool) = toggle_class ?force elt _class in
  ()

let has_class (elt : #Dom_html.element Js.t) (_class : string) : bool =
  Js.to_bool @@ elt##.classList##contains (Js.string _class)

let query_selector (elt : #Dom_html.element Js.t) (selector : string) : t option =
  Js.Opt.to_option @@ elt##querySelector (Js.string selector)

let query_selector_exn (elt : #Dom_html.element Js.t) (selector : string) : t =
  Js.Opt.get
    (elt##querySelector (Js.string selector))
    (fun () -> failwith (Printf.sprintf "no element matches selector `%s`" selector))

let query_selector_all (elt : #Dom_html.element Js.t) (selector : string) : t list =
  Dom.list_of_nodeList @@ elt##querySelectorAll (Js.string selector)

let get_attribute (elt : #Dom_html.element Js.t) (a : string) : string option =
  elt##getAttribute (Js.string a)
  |> Js.Opt.to_option
  |> function
  | None -> None
  | Some x -> Some (Js.to_string x)

let has_attribute (elt : #Dom_html.element Js.t) (a : string) : bool =
  Js.to_bool @@ elt##hasAttribute (Js.string a)

let set_attribute (elt : #Dom_html.element Js.t) (a : string) (v : string) : unit =
  elt##setAttribute (Js.string a) (Js.string v)

let remove_attribute (elt : #Dom_html.element Js.t) (a : string) : unit =
  elt##removeAttribute (Js.string a)

let set_style_property (elt : #Dom_html.element Js.t) (prop : string) (value : string) :
    unit =
  (Js.Unsafe.coerce elt##.style)##setProperty (Js.string prop) (Js.string value)

let get_parent (elt : #Dom_html.element Js.t) : t Js.opt =
  if Js.Optdef.test (Js.Unsafe.coerce elt)##.parentElement
  then (Js.Unsafe.coerce elt)##.parentElement
  else
    Js.Opt.bind elt##.parentNode (fun (p : Dom.node Js.t) ->
        match p##.nodeType with
        | ELEMENT -> Js.some (Js.Unsafe.coerce p)
        | _ -> Js.null)

let get_next_sibling (elt : #Dom_html.element Js.t) : t Js.opt =
  (Js.Unsafe.coerce elt)##.nextElementSibling

let matches (e : #Dom_html.element Js.t) (selector : string) : bool =
  let native_matches =
    if Js.Optdef.test (Js.Unsafe.coerce e)##.matches
    then (Js.Unsafe.coerce e)##.matches
    else if Js.Optdef.test (Js.Unsafe.coerce e)##.webkitMatchesSelector
    then (Js.Unsafe.coerce e)##.webkitMatchesSelector
    else (Js.Unsafe.coerce e)##.msMatchesSelector
  in
  Js.Unsafe.call native_matches e [|Js.Unsafe.inject (Js.string selector)|] |> Js.to_bool

let closest (e : t) (selector : string) : t Js.opt =
  if Js.Optdef.test (Js.Unsafe.coerce e)##.closest
  then (Js.Unsafe.coerce e)##closest (Js.string selector)
  else
    let rec aux (p : Dom_html.element Js.t Js.opt) =
      Js.Opt.bind p (fun (e : t) ->
          if matches e selector then Js.some e else aux (get_parent e))
    in
    aux (Js.some e)

let is_focus_inside (elt : #Dom_html.element Js.t) : bool =
  Js.Opt.map Dom_html.document##.activeElement (fun active ->
      Js.to_bool @@ (Js.Unsafe.coerce elt)##contains active)
  |> fun x -> Js.Opt.get x (fun () -> false)

let is_focused (elt : #Dom_html.element Js.t) : bool =
  Js.Opt.map Dom_html.document##.activeElement (equal elt)
  |> fun x -> Js.Opt.get x (fun () -> false)

let is_scrollable (elt : #Dom_html.element Js.t) : bool =
  elt##.scrollHeight > elt##.offsetHeight

let find f (nodes : Dom_html.element Dom.nodeList Js.t) =
  let rec find = function
    | 0 -> Js.null
    | i ->
        let item =
          Js.Opt.bind (nodes##item (i - 1)) (fun e -> if f e then Js.some e else Js.null)
        in
        if Js.Opt.test item then item else find (pred i)
  in
  find nodes##.length

(* let emit ?(should_bubble = false) ?detail evt_type element =
 *   let (evt : 'a custom_event Js.t) =
 *     match Js.(to_string @@ typeof (Unsafe.global##._CustomEvent)) with
 *     | "function" ->
 *       let custom : (_ Dom_html.Event.typ -> _ Js.t -> _ custom_event Js.t) Js.constr =
 *         Js.Unsafe.global##.CustomEvent in
 *       let obj =
 *         object%js
 *           val detail = Js.Opt.option detail
 *           val bubbles = should_bubble
 *         end in
 *       new%js custom evt_type obj
 *     | _ ->
 *       let doc = Js.Unsafe.coerce Dom_html.document in
 *       let evt = doc##createEvent (Js.string "CustomEvent") in
 *       evt##initCustomEvent evt_type
 *         (Js.bool should_bubble)
 *         Js._false
 *         (Js.Opt.option detail) in
 *   (Js.Unsafe.coerce element)##dispatchEvent evt *)

let emit ?(should_bubble = false) ?detail evt_type (element : Dom_html.element Js.t) =
  let event = Dom_html.createCustomEvent ?detail ~bubbles:should_bubble evt_type in
  element##dispatchEvent (event :> Dom_html.event Js.t)

let remove (x : #Dom.node Js.t) =
  if Js.Optdef.test (Js.Unsafe.coerce x)##.remove
  then (Js.Unsafe.coerce x)##remove
  else Js.Opt.iter (get_parent x) (fun p -> Dom.removeChild p x)

let array_of_node_list (nodeList : 'a Dom.nodeList Js.t) =
  let length = nodeList##.length in
  Array.init length (fun i ->
      Js.Opt.case (nodeList##item i) (fun () -> assert false) (fun x -> x))

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
