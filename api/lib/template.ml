open Containers
open Interaction
open Common

let rec filter_map f = function
  | [] -> []
  | x :: xs ->
     match f x with
     | Some v -> v :: (filter_map f xs)
     | None -> filter_map f xs

let is_absolute : Uri.Path.t -> bool = function
  | [] -> true
  | hd :: _ ->
     try Char.equal String.(get hd 0) '/'
     with _ -> false

let is_absolute_ref : Uri.Path.t -> bool = function
  | [] -> true
  | hd :: _ ->
     try Char.equal (String.get hd 0) '/'
         && Char.equal (String.get hd 1) '/'
     with _ -> false

let make_absolute : Uri.Path.t -> Uri.Path.t = function
  | [] -> []
  | (hd :: tl) as path ->
     if is_absolute path
     then path
     else ("/" ^ hd) :: tl

let make_absolute_ref : Uri.Path.t -> Uri.Path.t = function
  | [] -> []
  | (hd :: tl) as path ->
     if is_absolute_ref path
     then path
     else ("//" ^ hd) :: tl

let elt_to_string elt =
  Format.asprintf "%a" (Tyxml.Xml.pp ()) elt

module Api_handler = Handler.Make(User)

type script = Src of string
            | Raw of string

type tmpl_props =
  { title : string option
  ; pre_scripts : script list
  ; post_scripts : script list
  ; stylesheets : string list
  ; content : string list
  }

type priority = [ `Index of int | `None ]

module Priority = struct
  type t = priority
  let compare a b =
    match a, b with
    | `None, `None -> 0
    | `None, _ -> -1
    | _    , `None -> 1
    | `Index x, `Index y -> Int.compare x y
  let equal a b =
    0 = compare a b
end

type upper = Loc_upper
type inner = Loc_inner

type _ item =
  | Home    : tmpl_props -> upper item
  | Pure    : { path : ('a,'b) Common.Uri.Path.Format.t
              ; template : tmpl_props } -> upper item
  | Ref     : { title : string
              ; href  : Common.Uri.Path.t
              ; absolute : bool } -> _ item
  | Simple  : { title : string
              ; icon  : Tyxml.Xml.elt option
              ; href  : Common.Uri.Path.t
              ; template : tmpl_props } -> _ item
  | Subtree : { title : string
              ; icon  : Tyxml.Xml.elt option
              ; href  : Common.Uri.Path.t
              ; templates : inner ordered_item list } -> upper item
and 'a ordered_item = (priority * 'a item)

let rec merge_subtree items =
  let subtree_eq priority title href = function
    | (prior, Subtree { title = name; href = hr; _ })
         when String.equal name title
              && Uri.Path.equal hr href
              && Priority.equal prior priority -> true
    | _ -> false in
  let subtree_merge acc = function
    | (_, Subtree { templates = ts; _ }) -> ts @ acc
    | _ -> failwith "merge_subtree: invariant broken" in
  match items with
  | []              -> []
  | (p, Subtree { title; icon; href; templates })::tl ->
     let related, rest = List.partition (subtree_eq p title href) tl in
     let templates = List.fold_left subtree_merge templates related
     in
     (p, Subtree { title; icon; href; templates }) :: (merge_subtree rest)
  | h :: tl -> h :: (merge_subtree tl)

let make_template (props : tmpl_props) =
  let script_to_object = function
    | Raw s -> `O [ "script", `String s; "src", `Bool false ]
    | Src s -> `O [ "script", `String s; "src", `Bool true  ] in
  [ "title",        `String (match props.title with
                             | None   -> "АТС-3"
                             | Some t -> t)
  ; "pre_scripts",  `A (List.map script_to_object props.pre_scripts)
  ; "post_scripts", `A (List.map script_to_object props.post_scripts)
  ; "stylesheets",  `A (List.map (fun x -> `O ["stylesheet", `String x]) props.stylesheets)
  ; "content",      `A (List.map (fun x -> `O ["element", `String x]) props.content) ]

let make_subtree href (subitems : inner ordered_item list) =
  let make_ref (_,v) =
    match v with
    | Simple x ->
       `O [ "title", `String x.title
          ; "icon", (match x.icon with
                     | Some e -> `O ["value", `String (elt_to_string e)]
                     | None -> `Null)
          ; "href",  `String (Uri.Path.to_string @@ make_absolute (href :: x.href)) ]
    | Ref x ->
       `O [ "title", `String x.title
          ; "href", `String (if x.absolute
                             then Uri.Path.to_string @@ make_absolute_ref x.href
                             else Uri.Path.to_string @@ make_absolute x.href) ]
    | _ -> `Null
  in List.map make_ref subitems

let make_item (_, v) =
  match v with
  | Home _ -> None
  | Pure _ -> None
  | Ref r ->
     let href =
       if r.absolute
       then Uri.Path.to_string @@ make_absolute_ref r.href
       else Uri.Path.to_string @@ make_absolute r.href in
     Some (`O [ "title", `String r.title
              ; "href", `String href
              ; "simple", `Bool true ])
  | Simple s ->
     let icon = match s.icon with
       | Some e -> `O ["value", `String (elt_to_string e)]
       | None   -> `Null in
     Some (`O [ "title", `String s.title
              ; "icon", icon
              ; "href", `String (Uri.Path.to_string @@ make_absolute s.href)
              ; "simple", `Bool true ])
  | Subtree s ->
     let icon = match s.icon with
       | Some e -> `O ["value", `String (elt_to_string e)]
       | None   -> `Null in
     let subtree = make_subtree (Uri.Path.to_string s.href) s.templates in
     Some (`O [ "title", `String s.title
              ; "icon", icon
              ; "href", `Null
              ; "subtree", `A subtree
              ; "simple", `Bool false])

let sort_items items =
  let compare : type a. a ordered_item -> a ordered_item -> int =
    fun (x,_) (y,_) -> Priority.compare x y in
  let subsorted =
    List.map (function
        | (p, Subtree s) ->
           let templates = List.sort compare s.templates in
           (p, Subtree { s with templates })
        | x -> x)
      items
  in List.sort compare subsorted

let make_node path tmpl : 'a Uri.Dispatcher.node =
  let templ, doc = match path with
    | `Fmt x -> Uri.Path.Format.to_templ x, Uri.Path.Format.doc x
    | `Path x -> Uri.Path.to_templ x, Uri.Path.to_string x in
  { docstring = None
  ; templ
  ; path_typ = doc
  ; query_typ = []
  ; handler = fun _ -> respond_string tmpl ()
  }

let build_templates ?(href_base = "") mustache_tmpl user
      (vals : upper ordered_item list) =
  let vals = sort_items @@ merge_subtree vals in
  let mustache_tmpl = Mustache.of_string mustache_tmpl in
  let items =
    [ "user", `String user
    ; "navigation",
      `A (List.fold_left
            (fun acc v ->
              match make_item v with
              | None -> acc
              | Some v -> v :: acc) [] vals
          |> List.rev) ] in
  let fill_in_sub base_title base_href = function
    | _, Simple { title; href; template; _ } ->
       let path = base_href @ href in
       let template = match template.title with
         | None -> template
         | Some t ->
            let title = base_title ^ " / " ^ t in
            { template with title = Some title } in
       Mustache.render mustache_tmpl (`O (items @ make_template template))
       |> make_node (`Path path)
       |> Option.return
    | _ -> None in
  let fill_in (_, v) =
    match v with
    | Ref r -> [ ]
    | Home t ->
       Mustache.render mustache_tmpl (`O (items @ make_template t))
       |> make_node (`Path Uri.Path.empty)
       |> List.pure
    | Pure s ->
       Mustache.render mustache_tmpl (`O (items @ make_template s.template))
       |> make_node (`Fmt s.path)
       |> List.pure
    | Simple s ->
       Mustache.render mustache_tmpl (`O (items @ make_template s.template))
       |> make_node (`Path s.href)
       |> List.pure
    | Subtree s ->
       List.filter_map (fill_in_sub s.title s.href) s.templates
  in
  List.fold_left (fun acc v -> (fill_in v) @ acc) [] vals

let build_route_table ?(href_base="") template user vals =
  let user = Common.User.to_string user in
  let pages = build_templates ~href_base template user vals in
  let empty = Uri.Dispatcher.empty in
  let tbl = List.fold_left Uri.Dispatcher.add empty pages in
  tbl
