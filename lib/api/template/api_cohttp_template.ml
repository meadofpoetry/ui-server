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

  let respond_string ?(status = `OK) body () =
    let open Lwt.Infix in
    Cohttp_lwt_unix.Server.respond_string ~status ~body ()
    >>= fun resp -> Lwt.return (`Response resp)

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
           | Some `String s -> Lwt.return (`Instant (respond_string s ()))
           | _ -> Lwt.return (`Error "unknown template error"))

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
