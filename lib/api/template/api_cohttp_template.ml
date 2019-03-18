open Netlib

module type USER = sig
  include Api.USER
  val to_string : t -> string
end
   
module Make (User : USER) = struct

  type user = User.t

  type script = Src of string
              | Raw of string

  type template_props =
    { title : string option
    ; pre_scripts : script list
    ; post_scripts : script list
    ; stylesheets : string list
    ; content : string list
    }

  type topmost
     
  type inner

  type priority = [ `Index of int | `None ]

  type 'a item_local =
    Item : ('b, user -> Mustache.Json.value option) Netlib.Uri.Path.Format.t (* PATH *)
           * (user -> Mustache.Json.value option) (* Item generator *)
           * (user -> Mustache.Json.value option) (* Content generator, 'b if args will be needed *)
           -> 'a item_local

  type 'a item = priority * 'a item_local

  let priority_compare a b =
    match a, b with
    | `None, `None -> 0
    | `None, _ -> -1
    | _    , `None -> 1
    | `Index x, `Index y -> compare x y

  let priority_compare_pair (a,_) (b,_) = priority_compare a b

  let elt_to_string elt =
    Format.asprintf "%a" (Tyxml.Xml.pp ()) elt

  let respond_string ?(status = `OK) body =
    Cohttp_lwt_unix.Server.respond_string ~status ~body

  let list_max ~default c = function
    | [] -> default
    | [x] -> x
    | h::tl -> List.fold_left (fun acc x -> if c x acc > 0 then x else acc) h tl

  let make_template (user : user) (props : template_props) =
    let script_to_object = function
      | Raw s -> `O [ "script", `String s; "src", `Bool false ]
      | Src s -> `O [ "script", `String s; "src", `Bool true  ] in
    [ "title",        `String (match props.title with
                               | None   -> "АТС-3"
                               | Some t -> t)
    ; "user",         `String (User.to_string user)
    ; "pre_scripts",  `A (List.map script_to_object props.pre_scripts)
    ; "post_scripts", `A (List.map script_to_object props.post_scripts)
    ; "stylesheets",  `A (List.map (fun x -> `O ["stylesheet", `String x]) props.stylesheets)
    ; "content",      `A (List.map (fun x -> `O ["element", `String x]) props.content) ]

  let make_icon icon =
    match icon with
    | Some e -> `O ["value", `String (elt_to_string e)]
    | None   -> `Null

  let make_subitems user lst : Mustache.Json.value list =
    let convert (Item (_, it, _)) =
      match it user with
      | None -> `Null
      | Some v -> v
    in List.map convert lst

  let make_node ~template paths (Item (path, items, cont)) =
    let table = Hashtbl.create 16 in
    (* Check if the same path was added twice *)
    assert (Uri.Path.Format.has_template paths path);
    Uri.Path.Format.store_template paths path;
    let generate =
      let gen_template _args user : Mustache.Json.value option = (* Use args if memoized templates are different *)
        match Hashtbl.find_opt table user with
        | Some v -> Some (`String v)
        | None ->
           let params =
             match items user, cont user with
             | Some `O i, Some `O c -> `O (i @ c)
             | Some (`O _ as i), _ -> i
             | _, Some (`O _ as c) -> c
             | _ -> `O []
           in
           let v = Mustache.render template params in
           Hashtbl.replace table user v;
           Some (`String v)
      in Uri.Path.Format.kprint gen_template path
    in
    Uri.Dispatcher.make ~path ~query:Uri.Query.[] generate
    |> Uri.Dispatcher.map_node (fun f ->
           fun user _body _env _state ->
           match f user with
           | Some `String s -> `Instant (respond_string s ())
           | _ -> `Error "unknown template error")

  let reference ?(restrict=[]) ?(priority=`None) ~title ~href =
    let not_allowed id = List.exists (User.equal id) restrict in
    let item user =
      if not_allowed user
      then None
      else
        Some (`O [ "title",  `String title
                 ; "href",   `String (Uri.to_string href)
                 ; "simple", `Bool true
          ])
    in
    let it = Item ( Uri.Path.Format.of_string @@ Uri.to_string href
                  , item
                  , fun _ -> None)
    in [ priority, it ]
       
  let simple ?(restrict=[]) ?(priority=`None) ~title ?icon ~path props =
    let not_allowed id = List.exists (User.equal id) restrict in
    let item user =
      if not_allowed user
      then None
      else
        Some (`O [ "title", `String title
                 ; "icon", (make_icon icon)
                 ; "href", `String (Uri.Path.to_string path)
                 ; "simple", `Bool true ])
    in
    let content user =
      if not_allowed user
      then None
      else Some (`O (make_template user props))
    in
    let it = Item ( Uri.Path.Format.of_string @@ Uri.Path.to_string path
                  , item
                  , content)
    in [ priority, it ]

  let parametric ?(restrict=[]) ~path  props : 'a item list =
    let not_allowed id = List.exists (User.equal id) restrict in
    let content user =
      if not_allowed user
      then None
      else Some (`O (make_template user props))
    in
    let it = Item ( path
                  , (fun _ -> None)
                  , content)
    in [ `None, it ]

  let subtree ?(restrict=[]) ?priority ~title ?icon (list : inner item list) =
    let not_allowed id = List.exists (User.equal id) restrict in
    let priority = match priority with
      | Some v -> v
      | None -> list_max ~default:`None priority_compare (List.map fst list)
    in
    let list =
      list
      |> List.sort priority_compare_pair
      |> List.map snd
    in
    let item user : Mustache.Json.value option =
      if not_allowed user
      then None
      else
        let subtree = make_subitems user list in
        Some (`O [ "title", `String title
                 ; "icon", (make_icon icon)
                 ; "simple", `Bool false
                 ; "subtree", `A subtree ])
    in
    List.map (fun (Item (f,_,co)) -> priority, Item (f,item,co)) list

  let make ~template list =
    let template = Mustache.of_string template in
    let paths = Uri.Path.Format.templates () in
    let list =
      list
      |> List.sort priority_compare_pair
      |> List.map snd
    in
    List.map (fun node -> `GET, make_node paths ~template node) list
                       
end
       
   (*
type resp = (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

type topmost

type inner       

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

type _ item = user -> Mustache.Json.t

module Priority = struct
  type t = priority
  let compare a b =
    match a, b with
    | `None, `None -> 0
    | `None, _ -> -1
    | _    , `None -> 1
    | `Index x, `Index y -> compare x y
  let equal a b =
    0 = compare a b
end

let reference ~title ~href =
  
   
(* TODO 4.08 *)
let option_map f = function
  | None -> None
  | Some x -> Some (f x)

let some x = Some x

let list x = [x]

let filter_map f lst =
  let rec loop acc = function
    | [] -> List.rev acc
    | h::tl ->
       match f h with
       | None -> loop acc tl
       | Some x -> loop (x::acc) tl
  in loop [] lst
   
let elt_to_string elt =
  Format.asprintf "%a" (Tyxml.Xml.pp ()) elt

let respond_string ?(status = `OK) body =
  Cohttp_lwt_unix.Server.respond_string ~status ~body
  
(*module Api_handler = Handler.Make(User)*)

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
    | `Index x, `Index y -> compare x y
  let equal a b =
    0 = compare a b
end

type topmost
type inner

type _ item =
  | Home    : tmpl_props -> topmost item
  | Pure    : { path : (unit -> resp, resp) Uri.Path.Format.t
              ; template : tmpl_props
              } -> topmost item
  | Ref     : { title : string
              ; href  : Uri.t
              } -> _ item
  | Simple  : { title : string
              ; icon  : Tyxml.Xml.elt option
              ; href  : Uri.Path.t
              ; template : tmpl_props
              } -> _ item
  | Subtree : { title : string
              ; icon  : Tyxml.Xml.elt option
              ; href  : Uri.Path.t
              ; templates : inner ordered_item list
              } -> topmost item
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
          ; "href",  `String (Uri.Path.(to_string @@ concat href x.href)) ]
    | Ref x ->
       `O [ "title", `String x.title
          ; "href", `String (Uri.to_string x.href) ]
    | _ -> `Null
  in List.map make_ref subitems

let make_item (_, v) =
  match v with
  | Home _ -> None
  | Pure _ -> None
  | Ref r ->
     let href = Uri.to_string r.href in
     Some (`O [ "title", `String r.title
              ; "href", `String href
              ; "simple", `Bool true ])
  | Simple s ->
     let icon = match s.icon with
       | Some e -> `O ["value", `String (elt_to_string e)]
       | None   -> `Null in
     Some (`O [ "title", `String s.title
              ; "icon", icon
              ; "href", `String (Uri.Path.to_string s.href)
              ; "simple", `Bool true ])
  | Subtree s ->
     let icon = match s.icon with
       | Some e -> `O ["value", `String (elt_to_string e)]
       | None   -> `Null in
     let subtree = make_subtree s.href s.templates in
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
  match path with
  | `Path x ->
     let handler = respond_string tmpl () in
     let path = Uri.Path.(Format.of_string @@ to_string x) in
     let doc = Uri.Path.to_string x in
     Uri.Dispatcher.make ~docstring:doc ~path ~query:Uri.Query.empty handler
  | `Fmt x ->
     let handler _ = respond_string tmpl () in
     let doc = Uri.Path.Format.doc x in
     Uri.Dispatcher.make ~docstring:doc ~path:x ~query:Uri.Query.empty handler

[@@@warning "-27"]
let build_templates ?(uri=Uri.empty) mustache_tmpl user
      (vals : topmost ordered_item list) =
  (*let path = Uri.Path.of_uri uri in*)
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
    | _, Simple { title = _; href; template; _ } ->
       let path = Uri.Path.concat base_href href in
       let template = match template.title with
         | None -> template
         | Some t ->
            let title = base_title ^ " / " ^ t in
            { template with title = Some title } in
       Mustache.render mustache_tmpl (`O (items @ make_template template))
       |> make_node (`Path path)
       |> some
    | _ -> None in
  let fill_in (_, v) =
    match v with
    | Ref _r -> [ ]
    | Home t ->
       Mustache.render mustache_tmpl (`O (items @ make_template t))
       |> make_node (`Path Uri.Path.empty)
       |> list
    | Pure s ->
       Mustache.render mustache_tmpl (`O (items @ make_template s.template))
       |> make_node (`Fmt s.path)
       |> list
    | Simple s ->
       Mustache.render mustache_tmpl (`O (items @ make_template s.template))
       |> make_node (`Path s.href)
       |> list
    | Subtree s ->
       filter_map (fill_in_sub s.title s.href) s.templates
  in
  List.fold_left (fun acc v -> (fill_in v) @ acc) [] vals

let build_route_table ?href_base ~template ~user vals =
  let href_base = option_map Uri.of_string href_base in
  let pages = build_templates ?uri:href_base template user vals in
  let empty = Uri.Dispatcher.empty in
  let tbl = List.fold_left Uri.Dispatcher.add empty pages in
  tbl

    *)
