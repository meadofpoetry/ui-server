open Containers
open Interaction

(* TODO remove this *)
let (=) = Pervasives.(=)
 
let rec filter_map f = function
  | []    -> []
  | x::xs ->
     match f x with
     | Some v -> v::(filter_map f xs)
     | None   -> filter_map f xs

let path_abs_string p     = Path.to_string @@ Path.make_absolute p
let path_abs_ref_string p = Path.to_string @@ Path.make_absolute_ref p
let path_abs_concat pl    = Path.to_string @@ Path.make_absolute @@ Path.concat pl

let elt_to_string elt =
  Format.asprintf "%a" (Tyxml.Xml.pp ()) elt

type script = Src of string
            | Raw of string

type tmpl_props =
  { title        : string option
  ; pre_scripts  : script list
  ; post_scripts : script list
  ; stylesheets  : string list
  ; content      : string list
  }

type priority = [ `Index of int | `None ]

module Priority = struct
  type t = priority
  let compare a b =
    match a, b with
    | `None, `None -> 0
    | `None, _     -> -1
    | _    , `None -> 1
    | `Index x, `Index y -> compare x y
end
                
type upper = Loc_upper
type inner = Loc_inner

type _ item =
  | Home    : tmpl_props -> upper item
  | Ref     : { title : string
              ; href  : Path.t
              ; absolute : bool } -> _ item
  | Simple  : { title : string
              ; icon  : Tyxml.Xml.elt option
              ; href  : Path.t
              ; template : tmpl_props } -> _ item
  | Subtree : { title : string
              ; icon  : Tyxml.Xml.elt option
              ; href  : Path.t
              ; templates : inner ordered_item list } -> upper item
and 'a ordered_item = (priority * 'a item)

let rec merge_subtree items =
  let subtree_eq priority title href = function
    | (prior, Subtree { title = name; href = hr; _ })
         when name = title && hr = href && prior = priority -> true
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
  ; "stylesheets",  `A (List.map (fun x -> `O [ "stylesheet", `String x ]) props.stylesheets)
  ; "content",      `A (List.map (fun x -> `O [ "element", `String x]) props.content) ]

let make_subtree href (subitems : inner ordered_item list) =
  let make_ref (_,v) =
    match v with
    | Simple x ->
       `O [ "title", `String x.title
          ; "icon",  (match x.icon with
                      | Some e -> `O ["value", `String (elt_to_string e)]
                      | None   -> `Null)
          ; "href",  `String (path_abs_concat [href; x.href]) ]
    | Ref x ->
       `O [ "title", `String x.title
          ; "href",  `String (if x.absolute
                              then path_abs_ref_string x.href
                              else path_abs_string x.href) ]
  in List.map make_ref subitems

let make_item (_, v) =
  match v with
  | Home _ -> None
  | Ref  r ->
     Some (`O [ "title",   `String r.title
              ; "href",    `String (if r.absolute
                                    then path_abs_ref_string r.href
                                    else path_abs_string r.href)
              ; "simple",  `Bool true ])
  | Simple s ->
     Some (`O [ "title",   `String s.title
              ; "icon",    (match s.icon with
                            | Some e -> `O ["value", `String (elt_to_string e)]
                            | None   -> `Null)
              ; "href",    `String (path_abs_string s.href)
              ; "simple",  `Bool true ])
  | Subtree s ->
     Some (`O [ "title",   `String s.title
              ; "icon",    (match s.icon with
                            | Some e -> `O ["value", `String (elt_to_string e)]
                            | None   -> `Null)
              ; "href",    `Null
              ; "subtree", `A (make_subtree s.href s.templates)
              ; "simple",  `Bool false])

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

let build_templates ?(href_base="") mustache_tmpl user (vals : upper ordered_item list) =
  let vals          = sort_items @@ merge_subtree vals in
  let mustache_tmpl = Mustache.of_string mustache_tmpl in
  let items         =
    [ "user",       `String user
    ; "navigation", `A (List.rev @@ List.fold_left
                                      (fun acc v ->
                                        match make_item v with
                                        | None -> acc
                                        | Some v -> v::acc) [] vals) ]
  in
  let fill_in_sub base_title base_href = function
    | _, Simple { title; href; template; _ } ->
       Some (Path.concat [base_href;href],
             let template = match template.title with
               | None -> template
               | Some t ->
                  let title = base_title ^ " / " ^ t in
                  { template with title = Some title } in
             Mustache.render mustache_tmpl (`O (items @ make_template template)))
    | _                                 -> None
  in
  let fill_in (_, v) =
    match v with
    | Ref  r    -> [ ]
    | Home t    -> [ Path.empty, (Mustache.render mustache_tmpl (`O (items @ make_template t)))]
    | Simple s  -> [ s.href,     (Mustache.render mustache_tmpl (`O (items @ make_template s.template)))]
    | Subtree s -> List.filter_map (fill_in_sub s.title s.href) s.templates
  in List.fold_left (fun acc v -> (fill_in v) @ acc) [] vals

type route_table = (string, string) Hashtbl.t (* TODO: proper hash *)

exception Path_not_uniq of string
                         
let rec uniq_paths = function
  | [] -> ()
  | (path, _)::tl ->
     match List.find_opt (fun (p,_) -> Path.equal path p) tl with
     | Some _ -> raise (Path_not_uniq (Path.to_string path))
     | None   -> uniq_paths tl
               
let build_route_table ?(href_base="") template user vals =
  let pages = build_templates ~href_base template user vals in
  uniq_paths pages;
  let tbl : route_table = Hashtbl.create 20 in
  List.iter (fun (path, page) -> Hashtbl.add tbl (Path.to_string path) page) pages;
  tbl
