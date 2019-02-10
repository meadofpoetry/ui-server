open Js_of_ocaml

type t = Dom_html.element Js.t

let equal (a : #Dom_html.element Js.t as 'a) (b : 'a) : bool =
  CCEqual.physical a b

let coerce (elt : #Dom_html.element Js.t) : t =
  (elt :> t)

let add_class (elt : #Dom_html.element Js.t) (_class : string) : unit =
  elt##.classList##add (Js.string _class)

let remove_class (elt : #Dom_html.element Js.t) (_class : string) : unit =
  elt##.classList##remove (Js.string _class)

let toggle_class ?(force : bool option)
      (elt : #Dom_html.element Js.t) (_class : string) : bool =
  match force with
  | None -> Js.to_bool @@ elt##.classList##toggle (Js.string _class)
  | Some true -> add_class elt _class; true
  | Some false -> remove_class elt _class; false

let has_class (elt : #Dom_html.element Js.t) (_class : string) : bool =
  Js.to_bool @@ elt##.classList##contains (Js.string _class)

let query_selector (elt : #Dom_html.element Js.t)
      (selector : string) : t option =
  Js.Opt.to_option @@ elt##querySelector (Js.string selector)

let get_attribute (elt : #Dom_html.element Js.t)
      (a : string) : string option =
  elt##getAttribute (Js.string a)
  |> Js.Opt.to_option
  |> function None -> None | Some x -> Some (Js.to_string x)

let set_attribute (elt : #Dom_html.element Js.t)
      (a : string) (v : string) : unit =
  elt##setAttribute (Js.string a) (Js.string v)

let remove_children (elt : #Dom_html.element Js.t) =
  Dom.list_of_nodeList @@ elt##.childNodes
  |> List.iter (fun x -> Dom.removeChild elt x)

let insert_child_at_index (parent : #Dom.node Js.t)
      (index : int) (child : #Dom.node Js.t) =
  let sibling = parent##.childNodes##item index in
  Dom.insertBefore parent child sibling

let set_style_property (elt : #Dom_html.element Js.t)
      (prop : string) (value : string) : unit =
  (Js.Unsafe.coerce elt##.style)##setProperty
    (Js.string prop) (Js.string value)
