open Interaction

let path_abs_string p = Path.to_string @@ Path.make_absolute p
let path_abs_ref_string p = Path.to_string @@ Path.make_absolute_ref p
let path_abs_concat pl = Path.to_string @@ Path.make_absolute @@ Path.concat pl
   
type script = Src of string
            | Raw of string

type tmpl_props =
  { title        : string option
  ; pre_scripts  : script list
  ; post_scripts : script list
  ; stylesheets  : string list
  ; content      : string list
  }

type upper = Loc_upper
type inner = Loc_inner
type _ item =
  | Home    : tmpl_props -> upper item
  | Ref     : { title : string; href : Path.t; absolute : bool } -> _ item
  | Simple  : { title : string; href : Path.t; template : tmpl_props } -> _ item
  | Subtree : { title : string; href : Path.t; templates : inner item list } -> upper item

let rec merge_subtree = function
  | []              -> []
  | (Ref r)::tl     -> (Ref r)::(merge_subtree tl)
  | (Simple s)::tl  -> (Simple s)::(merge_subtree tl)
  | (Home t)::tl    -> (Home t)::(merge_subtree tl)
  | (Subtree { title; href; templates })::tl ->
     let related, rest = List.partition
                           (function Subtree { title = name; href = hr; _ }
                                 when name = title && hr = href -> true
                                   | _ -> false)
                           tl in
     let templates = List.fold_left
                       (fun acc s ->
                         match s with
                         | (Subtree { title = _; href = _; templates = ts }) -> ts @ acc
                         | _ -> failwith "merge_subtree: invariant broken")
                       templates related
     in (Subtree { title; href; templates }) :: (merge_subtree rest)

let make_template (props : tmpl_props) =
  let script_to_object = function
    | Raw s -> `O [ "script", `String s; "src", `Bool false ]
    | Src s -> `O [ "script", `String s; "src", `Bool true  ] in
  [ "title",        `String (match props.title with None -> "АТС-3" | Some t -> t)
  ; "pre_scripts",  `A (List.map script_to_object props.pre_scripts)
  ; "post_scripts", `A (List.map script_to_object props.post_scripts)
  ; "stylesheets",  `A (List.map (fun x -> `O [ "stylesheet", `String x ]) props.stylesheets)
  ; "content",      `A (List.map (fun x -> `O [ "element", `String x]) props.content) ]

let make_subtree href (subitems : inner item list) =
  List.map (function
      | Simple x -> `O [ "title", `String x.title
                       ; "href", `String (path_abs_concat [href; x.href]) ]
      | Ref x -> `O [ "title", `String x.title
                    ; "href", `String (if x.absolute
                                       then path_abs_ref_string x.href
                                       else path_abs_string x.href) ] )
    subitems

let make_item = function
  | Home _          -> None
  | Ref  r          -> Some (`O [ "title",   `String r.title
                                ; "href",    `String (if r.absolute
                                                      then path_abs_ref_string r.href
                                                      else path_abs_string r.href)
                                ; "simple",  `Bool true ])
  | Simple s        -> Some (`O [ "title",   `String s.title
                                ; "href",    `String (path_abs_string s.href)
                                ; "simple",  `Bool true ])
  | Subtree s       -> Some (`O [ "title",   `String s.title
                                ; "href",    `Null
                                ; "subtree", `A (make_subtree s.href s.templates)
                                ; "simple",  `Bool false])

let build_templates ?(href_base="") mustache_tmpl vals =
  let vals          = merge_subtree vals in
  let mustache_tmpl = Mustache.of_string mustache_tmpl in
  let items         = [ "navigation",
                        `A (List.rev @@ List.fold_left
                                          (fun acc v ->
                                            match make_item v with
                                            | None -> acc
                                            | Some v -> v::acc) [] vals) ] in
  let fill_in v =
    match v with
    | Ref  r    -> [ ]
    | Home t    -> [ Path.empty, (Mustache.render mustache_tmpl (`O (items @ make_template t)))]
    | Simple s  -> [ s.href,     (Mustache.render mustache_tmpl (`O (items @ make_template s.template)))]
    | Subtree s ->
       CCList.filter_map (function
           | Simple { title;href;template } ->
              Some (Path.concat [s.href;href],(Mustache.render mustache_tmpl (`O (items @ make_template template))))
           | _                              -> None)
         s.templates
  in List.fold_left (fun acc v -> (fill_in v) @ acc) [] vals

type route_table = (string, string) Hashtbl.t (* TODO: proper hash *)

exception Path_not_uniq of string
                 
let rec uniq_paths = function
  | [] -> ()
  | (path, _)::tl ->
     match List.find_opt (fun (p,_) -> path = p) tl with
     | Some _ -> raise (Path_not_uniq (Path.to_string path))
     | None   -> uniq_paths tl
                 
let build_root_table ?(href_base="") template vals =
  let pages = build_templates ~href_base template vals in
  uniq_paths pages;
  let tbl : route_table = Hashtbl.create 20 in
  List.iter (fun (path, page) -> Hashtbl.add tbl (Path.to_string path) page) pages;
  tbl
