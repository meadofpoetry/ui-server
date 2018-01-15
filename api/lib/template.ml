open Interaction

type script = Src of string
            | Raw of string

type tmpl_props =
  { title        : string option
  ; pre_scripts  : script list
  ; post_scripts : script list
  ; stylesheets  : string list
  ; content      : string list
  }
  
type item =
  | Ref     of string * string
  | Home    of tmpl_props
  | Simple  of { title : string; template : tmpl_props }
  | Subtree of { title : string; templates : (string * tmpl_props) list }
             
let rec merge_subtree = function
  | []              -> []
  | (Ref (n,r))::tl -> (Ref (n,r))::(merge_subtree tl)
  | (Simple s)::tl  -> (Simple s)::(merge_subtree tl)
  | (Home t)::tl    -> (Home t)::(merge_subtree tl)
  | (Subtree { title; templates })::tl ->
     let related, rest = List.partition
                           (function Subtree { title = name; templates = ts } when name = title -> true
                                   | _ -> false)
                           tl in
     let templates = List.fold_left
                       (fun acc s ->
                         match s with
                         | (Subtree { title = _; templates = ts }) -> ts @ acc
                         | _ -> failwith "merge_subtree: invariant broken")
                       [] related
     in (Subtree { title; templates }) :: (merge_subtree rest)

let make_template props =
  let script_to_object = function
    | Raw s -> `O [ "script", `String s; "src", `Bool false ]
    | Src s -> `O [ "script", `String s; "src", `Bool true  ] in
  [ "title",        `String (match props.title with None -> "АТС-3" | Some t -> t)
  ; "pre_scripts",  `A (List.map script_to_object props.pre_scripts)
  ; "post_scripts", `A (List.map script_to_object props.post_scripts)
  ; "stylesheets",  `A (List.map (fun x -> `O [ "stylesheet", `String x ]) props.stylesheets)
  ; "content",      `A (List.map (fun x -> `O [ "element", `String x]) props.content) ]

let make_item = function
  | Home _          -> None
  | Ref (name, ref) -> None
  | Simple s        -> Some (`O [ "title", `String s.title; "href", `String s.title ])
  | Subtree _       -> None
      
let build_templates ?(href_base="") template vals =
  let vals     = merge_subtree vals in
  let template = Mustache.of_string template in
  let items    = [ "navigation",
                   `A (List.rev @@ List.fold_left
                         (fun acc v ->
                           match make_item v with
                           | None -> acc
                           | Some v -> v::acc) [] vals) ] in
  let fill_in v =
    match v with
    | Ref (n,_) -> [[n], None]
    | Home temp -> [[], Some (Mustache.render template (`O (items @ make_template temp)))]
    | Simple s  -> [[s.title], Some (Mustache.render template (`O (items @ make_template s.template)))]
    | Subtree s ->
       List.map (fun (sub,t) ->
           [s.title;sub], Some (Mustache.render template (`O (items @ make_template t))))
         s.templates
  in List.fold_left (fun acc v -> (fill_in v) @ acc) [] vals

type route_table = (string list, string option) Hashtbl.t (* TODO: proper hash *)
   
let build_root_table ?(href_base="") template vals =
  let pages = build_templates ~href_base template vals in
  let tbl : route_table = Hashtbl.create 20 in
  List.iter (fun (path, page) -> Hashtbl.add tbl path page) pages;
  tbl

   (*
let template ?(tmpl="base.html") path props =
  let tmpl = Filename.concat path ("html/templates/" ^ tmpl)
             |> CCIO.File.read_exn (* FIXME *)
             |> Mustache.of_string in
  try
    Mustache.render tmpl (fill_json props)
  with
  | e -> Printexc.to_string e
    *)
