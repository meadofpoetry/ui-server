open Js_of_ocaml

type node = Dom_html.element Js.t

let selectors =
  [ "input"
  ; "select"
  ; "textarea"
  ; "a[href]"
  ; "button"
  ; "[tabindex]"
  ; "audio[controls]"
  ; "video[controls]"
  ; "[contenteditable]:not([contenteditable=\"false\"])" ]

let selector = String.concat "," selectors

let focusable_candidate_selector = String.concat "," ("iframe" :: selectors)

let is_content_editable (elt : node) : bool =
  Js.string "true" == (Js.Unsafe.coerce elt)##.contentEditable

let coerce_to_input (elt : node) : Dom_html.inputElement Js.t option =
  match Js.to_string elt##.tagName with
  | "INPUT" -> Some (Js.Unsafe.coerce elt)
  | _ -> None

let is_hidden_input (elt : node) : bool =
  match coerce_to_input elt with
  | None -> false
  | Some i -> (
    match Js.to_string i##._type with
    | "hidden" -> true
    | _ -> false)

let is_radio (elt : node) : bool =
  match coerce_to_input elt with
  | None -> false
  | Some i -> (
    match Js.to_string i##._type with
    | "radio" -> true
    | _ -> false)

let get_checked_radio (nodes : Dom_html.element Dom.nodeList Js.t) : node option =
  List.find_opt (fun (elt : node) ->
      let (i : Dom_html.inputElement Js.t) = Js.Unsafe.coerce elt in
      Js.to_bool i##.checked)
  @@ Dom.list_of_nodeList nodes

let is_tabbable_radio (elt : node) : bool =
  match coerce_to_input elt with
  | None -> false
  | Some i -> (
    match Js.to_string i##.name with
    | "" -> true
    | name -> (
        let (owner : Dom_html.document Js.t) = (Js.Unsafe.coerce i)##.ownerDocument in
        let (selector : string) =
          Printf.sprintf "input[type=\"radio\"][name=\"%s\"]" name
        in
        let radio_set = owner##querySelectorAll (Js.string selector) in
        match get_checked_radio radio_set with
        | None -> true
        | Some x -> Element.equal x elt))

let is_non_tabbable_radio (elt : node) : bool =
  is_radio elt && (not @@ is_tabbable_radio elt)

let get_tab_index (elt : node) : int =
  let attr =
    match Element.get_attribute elt "tabindex" with
    | None -> None
    | Some x -> int_of_string_opt x
  in
  match attr with
  | Some x -> x
  | None ->
      (* Browsers do not return `tabIndex` correctly for contentEditable nodes;
          so if they don't have a tabindex attribute specifically set,
          assume it's 0. *)
      if is_content_editable elt then 0 else (Js.Unsafe.coerce elt)##.tabIndex

let is_hidden (elt : node) : bool =
  (not @@ Js.Opt.test elt##.offsetParent)
  ||
  match Js.to_string (Dom_html.window##getComputedStyle elt)##.visibility with
  | "hidden" -> true
  | _ -> false

let is_node_matching_selector_focusable (elt : node) : bool =
  let disabled =
    match Js.Optdef.to_option (Js.Unsafe.coerce elt)##.disabled with
    | None -> false
    | Some x -> Js.to_bool x
  in
  if disabled || is_hidden_input elt || is_hidden elt then false else true

let is_node_matching_selector_tabbable (elt : node) : bool =
  if (not @@ is_node_matching_selector_focusable elt)
     || is_non_tabbable_radio elt
     || get_tab_index elt < 0
  then false
  else true

let is_tabbable (elt : #Dom_html.element Js.t) : bool =
  if Element.matches elt selector
  then is_node_matching_selector_tabbable (Element.coerce elt)
  else false

let is_focusable (elt : #Dom_html.element Js.t) : bool =
  if Element.matches elt focusable_candidate_selector
  then is_node_matching_selector_focusable (Element.coerce elt)
  else false

let sort_ordered (a : int * int * node as 'a) (b : 'a) : int =
  let doc_order_1, tab_index1, _ = a in
  let doc_order_2, tab_index2, _ = b in
  if tab_index1 = tab_index2 then doc_order_1 - doc_order_2 else tab_index1 - tab_index2

let get_tabbable ?(include_container = false) (elt : #Dom_html.element Js.t) :
    Dom_html.element Js.t list =
  let rec loop regular ordered i = function
    | [] -> List.rev regular, List.rev ordered
    | item :: tl -> (
        if not @@ is_node_matching_selector_tabbable item
        then loop regular ordered (succ i) tl
        else
          match get_tab_index item with
          | 0 -> loop (item :: regular) ordered (succ i) tl
          | x -> loop regular ((i, x, item) :: ordered) (succ i) tl)
  in
  let cons_if x i l = if x then i :: l else l in
  let candidates =
    cons_if include_container (Element.coerce elt)
    @@ Dom.list_of_nodeList
    @@ elt##querySelectorAll (Js.string selector)
  in
  let regualar, ordered = loop [] [] 0 candidates in
  List.sort sort_ordered ordered |> List.map (fun (_, _, e) -> e) |> List.append regualar
