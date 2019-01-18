open Containers
open Interaction
open Common

let default_title = "АТС-3"

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

module Api_handler = Handler.Make(User)

type script =
  | Src of string (* Path to JavaScript file *)
  | Raw of string (* Raw JavaScript code *)

(**
 * leading - optional top app bar leading icon
 * title - optional page title
 * actions - optional top app bar actions
 *)
type app_bar_props =
  { leading : Tyxml.Xml.elt option
  ; title : string option
  ; actions : Tyxml.Xml.elt list
  ; bottom : Tyxml.Xml.elt option
  }

let make_app_bar_props ?leading ?title ?(actions = []) ?bottom () =
  { leading; title; actions; bottom }

(**
 * id - to find correspondence between a page and a navigation item
 * pre_scripts - scripts to include before page content, in a `head` tag
 * post_scripts - scripts to include after page content, before closing the `body` tag
 * stylesheets - CSS stylesheets to include
 * content - HTML elements to include into a page main content
 *)
type tmpl_props =
  { id : string option
  ; app_bar : app_bar_props option
  ; pre_scripts : script list
  ; post_scripts : script list
  ; stylesheets : string list
  ; content : Tyxml.Xml.elt list
  }

let make_tmpl_props ?id ?app_bar
      ?(pre_scripts = []) ?(post_scripts = []) ?(stylesheets = [])
      ?(content = []) () =
  { id; app_bar; pre_scripts; post_scripts; stylesheets; content }

type priority = [`Index of int | `None]

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

type ('a, 'b) pure =
  { path : ('a, 'b) Uri.Path.Format.t
  ; template : tmpl_props
  }

type reference =
  { title : string
  ; href : Uri.Path.t
  ; absolute : bool
  }

type simple =
  { id : string
  ; title : string
  ; icon : Tyxml.Xml.elt option
  ; href : Uri.Path.t
  ; template : tmpl_props
  }

type _ item =
  | Home : tmpl_props -> upper item
  | Pure : _ pure -> upper item
  | Ref : reference -> _ item
  | Simple : simple -> _ item
  | Subtree : { title : string
              ; icon : Tyxml.Xml.elt option
              ; href : Uri.Path.t
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
  | [] -> []
  | (p, Subtree { title; icon; href; templates })::tl ->
     let related, rest = List.partition (subtree_eq p title href) tl in
     let templates = List.fold_left subtree_merge templates related in
     (p, Subtree { title; icon; href; templates }) :: (merge_subtree rest)
  | h :: tl -> h :: (merge_subtree tl)

let elt_to_json (elt : Tyxml.Xml.elt) =
  `String (Format.asprintf "%a" (Tyxml.Xml.pp ()) elt)

let elt_to_json_obj (key : string) (elt : Tyxml.Xml.elt) =
  `O [key, elt_to_json elt]

let to_json_array f l = `A (List.map f l)

let make_template ({ pre_scripts
                   ; post_scripts
                   ; stylesheets
                   ; content
                   ; app_bar
                   ; _ } : tmpl_props) =
  let script_to_json = function
    | Raw s -> `O ["script", `String s; "src", `Bool false]
    | Src s -> `O ["script", `String s; "src", `Bool true] in
  let css_to_obj (s : string) = `O ["stylesheet", `String s] in
  let title = match app_bar with
    | None -> default_title
    | Some p -> Option.get_or ~default:default_title p.title in
  [ "title", `String title
  ; "pre_scripts", to_json_array script_to_json pre_scripts
  ; "post_scripts", to_json_array script_to_json post_scripts
  ; "stylesheets", to_json_array css_to_obj stylesheets
  ; "content", to_json_array (elt_to_json_obj "element") content
  ]

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
  in
  List.sort compare subsorted

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

let make_reference (r : reference) =
  let href =
    if r.absolute
    then Uri.Path.to_string @@ make_absolute_ref r.href
    else Uri.Path.to_string @@ make_absolute r.href in
  `O [ "title", `String r.title
     ; "active", `Bool false
     ; "href", `String href
     ; "simple", `Bool true ]

let make_simple ?(id : string option) ?href (s : simple) =
  let active = match id with
    | None -> false
    | Some id -> String.equal id s.id in
  let icon = match s.icon with
    | None -> `Null
    | Some e -> elt_to_json_obj "value" e in
  let href =
    Uri.Path.to_string
    @@ make_absolute
    @@ begin match href with
       | None -> make_absolute s.href
       | Some h -> h :: s.href
       end in
  `O [ "title", `String s.title
     ; "icon", icon
     ; "active", `Bool active
     ; "href", `String href
     ; "simple", `Bool true ]

let make_subtree ?id href (subitems : inner ordered_item list) =
  let make_ref (_, v) =
    match v with
    | Simple x -> make_simple ?id ~href x
    | Ref x -> make_reference x
    | _ -> `Null
  in List.map make_ref subitems

let make_item ?(id : string option) (_, (v : upper item))
  : Mustache.Json.value option =
  match v with
  | Home _ -> None
  | Pure _ -> None
  | Ref r ->
     Some (make_reference r)
  | Simple s -> Some (make_simple ?id s)
  | Subtree s ->
     let (icon : Mustache.Json.value) = match s.icon with
       | Some e -> elt_to_json_obj "value" e
       | None -> `Null in
     let subtree = make_subtree ?id (Uri.Path.to_string s.href) s.templates in
     Some (`O [ "title", `String s.title
              ; "icon", icon
              ; "href", `Null
              ; "subtree", `A subtree
              ; "simple", `Bool false])

module Icon = Components_markup.Icon.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)

let make_account_color (user : User.t) : string =
  Components_markup.Material_color_palette.(
    let fill = match user with
      | `Root -> Red C500
      | `Operator -> Orange C500
      | `Guest -> Light_green C500 in
    Color.to_css_rgba
    @@ Components_markup.Material_color_palette.make fill)

let make_account_icon (user : User.t) : string =
  let path' = Icon.SVG.Path.account in
  let path = Icon.SVG.create_path path' () in
  let icon = Icon.SVG.create [path] () in
  Format.asprintf "%a" (Tyxml.Html.pp_elt ()) icon

let make_navigation ?(id : string option) user (vals : ('a * upper item) list)
  : (string * Mustache.Json.value) list =
  let f acc v = List.cons_maybe (make_item ?id v) acc in
  let items = List.(rev @@ fold_left f [] vals) in
  [ "navigation", `A items
  ; "username", `String (User.to_human_string user)
  ; "usericon", `String (make_account_icon user)
  ; "usercolor", `String (make_account_color user)
  ]

let make_app_bar (props : app_bar_props option)
    : (string * Mustache.Json.value) list =
  match props with
  | None ->
     [ "title", `String default_title
     ; "custom_leading", `Bool false
     ; "has_actions", `Bool false
     ; "actions", `A []
     ; "has_bottom", `Bool false
     ]
  | Some p ->
     let leading = match p.leading with
       | None -> None
       | Some elt -> Some ("leading", elt_to_json elt) in
     let bottom = match p.bottom with
       | None -> None
       | Some elt -> Some ("bottom", elt_to_json elt) in
     [ "title", `String (Option.get_or ~default:default_title p.title)
     ; "custom_leading", `Bool (Option.is_some p.leading)
     ; "has_actions", `Bool (not @@ List.is_empty p.actions)
     ; "actions", to_json_array (elt_to_json_obj "action") p.actions
     ; "has_bottom", `Bool (Option.is_some p.bottom)
     ]
     |> List.cons_maybe leading
     |> List.cons_maybe bottom

type template_files =
  { base : string
  ; nav : string
  ; app_bar : string
  }

let render (files : template_files) (props : tmpl_props) user vals =
  let app_bar =
    `O (make_app_bar props.app_bar)
    |> Mustache.(render @@ of_string files.app_bar) in
  let nav =
    `O (make_navigation ?id:props.id user vals)
    |> Mustache.(render @@ of_string files.nav) in
  `O ([ "user", `String (User.to_string user)
      ; "app_bar", `String app_bar
      ; "navigation", `String nav
      ]
      @ (make_template props))
  |> Mustache.(render @@ of_string files.base)

let build_templates ?(href_base = "")
      (files : template_files)
      (user : User.t)
      (vals : upper ordered_item list) =
  let vals = sort_items @@ merge_subtree vals in
  let fill_in_sub base_title base_href = function
    | _, Simple { title; href; template; _ } ->
       let path = base_href @ href in
       let template = match template.app_bar with
         | None -> template
         | Some app_bar ->
            match app_bar.title with
            | None -> template
            | Some t ->
               let title = Some (base_title ^ " / " ^ t) in
               let app_bar = Some { app_bar with title } in
               { template with app_bar } in
       render files template user vals
       |> make_node (`Path path)
       |> Option.return
    | _ -> None in
  let fill_in (_, v) =
    match v with
    | Ref r -> []
    | Home t ->
       render files t user vals
       |> make_node (`Path Uri.Path.empty)
       |> List.pure
    | Pure s ->
       render files s.template user vals
       |> make_node (`Fmt s.path)
       |> List.pure
    | Simple s ->
       render files s.template user vals
       |> make_node (`Path s.href)
       |> List.pure
    | Subtree s ->
       List.filter_map (fill_in_sub s.title s.href) s.templates
  in
  List.fold_left (fun acc v -> (fill_in v) @ acc) [] vals

let build_route_table ?(href_base = "") (template : template_files)
      (user : User.t) vals =
  let pages = build_templates ~href_base template user vals in
  let empty = Uri.Dispatcher.empty in
  let tbl = List.fold_left Uri.Dispatcher.add empty pages in
  tbl
