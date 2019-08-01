open Netlib

module type USER = sig
  include Api.USER
  val to_string : t -> string
end

(* TODO remove after 4.08 *)
let filter_map f l =
  let rec aux acc = function
    | [] -> List.rev acc
    | x :: l' ->
      let acc' = match f x with | None -> acc | Some y -> y :: acc in
      aux acc' l'
  in aux [] l

module Make (User : USER) = struct

  type user = User.t

  type script = Src of string
              | Raw of string

  type side_sheet_props =
    { clipped : bool
    ; opened : bool
    ; typ : [`Modal | `Dismissible | `Permanent]
    ; content : Tyxml.Xml.elt list
    }

  type template_props =
    { title : string option
    ; top_app_bar_leading : Tyxml.Xml.elt option
    ; top_app_bar_content : Tyxml.Xml.elt list
    ; top_app_bar_bottom : Tyxml.Xml.elt option
    ; side_sheet : side_sheet_props option
    ; pre_scripts : script list
    ; post_scripts : script list
    ; stylesheets : string list
    ; content : Tyxml.Xml.elt list
    }

  let make_template_props ?title
      ?top_app_bar_leading ?(top_app_bar_content = []) ?top_app_bar_bottom
      ?side_sheet
      ?(pre_scripts = []) ?(post_scripts = []) ?(stylesheets = [])
      ?(content = []) () =
    { title
    ; top_app_bar_leading
    ; top_app_bar_content
    ; top_app_bar_bottom
    ; side_sheet
    ; pre_scripts
    ; post_scripts
    ; stylesheets
    ; content
    }

  type topmost

  type inner

  type priority = [ `Index of int | `None ]

  type 'a item_local =
      Item : { path : ('b, user -> gen_result) Netlib.Uri.Path.Format.t
             ; subitems : subitems
             ; item_gen : user -> gen_result
             ; page_gen : user -> gen_result
             } -> 'a item_local
  and subitems =
    [ `Subtree of inner item_local list
    | `Simple
    ]
  and gen_result =
    [ `Null
    | `Not_allowed
    | `Template of (string * Mustache.Json.value) list
    | `HTML of string
    ]

  type 'a item = priority * 'a item_local

  let make_side_sheet_props ?(clipped = true) ?(opened = false)
      ?(typ = `Dismissible) ?(content = []) () =
    { clipped; opened; typ; content }

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
    | h :: tl -> List.fold_left (fun acc x -> if c x acc > 0 then x else acc) h tl

  let make_top_app_bar ({ top_app_bar_leading = leading
                        ; top_app_bar_content = content
                        ; top_app_bar_bottom = bottom
                        ; _ } : template_props) =
    let leading' = match leading with
      | None ->
        elt_to_string
        @@ Tyxml_html.toelt
        @@ Tyxml.Svg.(
            Tyxml_html.svg
              ~a:[ a_class ["mdc-icon"]
                 ; a_style "width: 24px; height: 24px"
                 ; a_viewBox (0., 0., 24., 24.) ]
              [path ~a:[a_d "M3,6H21V8H3V6M3,\
                             11H21V13H3V11M3,\
                             16H21V18H3V16Z"]
                 []])
      | Some elt -> elt_to_string elt in
    let bottom' = match bottom with
      | None -> `String ""
      | Some elt -> `String (elt_to_string elt) in
    let content' = List.map (fun x ->
        `O ["element", `String (elt_to_string x)])
        content in
    [ "top_app_bar_leading", `String leading'
    ; "top_app_bar_content", `A content'
    ; "top_app_bar_bottom", bottom' ]

  let make_side_sheet = function
    | None -> [ "side_sheet_full_height", `String ""
              ; "side_sheet_clipped", `String "" ]
    | Some ({ content; opened; typ; clipped } : side_sheet_props) ->
      let make () =
        let class_ = match typ with
          | `Modal -> "mdc-side-sheet--modal"
          | `Dismissible -> "mdc-side-sheet--dismissible"
          | `Permanent -> "" in
        let opened = if opened then "mdc-side-sheet--opened" else "" in
        elt_to_string
        @@ Tyxml_html.toelt
        @@ Tyxml_html.(
            aside ~a:[a_class ["mdc-side-sheet"; class_; opened]]
              (List.map Tyxml.Html.tot content)) in
      match clipped with
      | false ->
        [ "side_sheet_full_height", `String (make ())
        ; "side_sheet_clipped", `String "" ]
      | true ->
        [ "side_sheet_full_height", `String ""
        ; "side_sheet_clipped", `String (make ()) ]

  let make_template (user : user)
      ({ pre_scripts
       ; post_scripts
       ; stylesheets
       ; title
       ; content
       ; side_sheet
       ; _ } as props : template_props) =
    let ( % ) f g x = f (g x) in
    let script_to_object = function
      | Raw s -> `O [ "script", `String s; "src", `Bool false ]
      | Src s -> `O [ "script", `String s; "src", `Bool true  ] in
    let make_obj k v = `O [k, `String v] in
    [ "title", `String (match title with
          | None -> "АТС-3"
          | Some t -> t)
    ; "user", `String (User.to_string user)
    ; "pre_scripts", `A (List.map script_to_object pre_scripts)
    ; "post_scripts", `A (List.map script_to_object post_scripts)
    ; "stylesheets", `A (List.map (make_obj "stylesheet") stylesheets)
    ; "content", `A (List.map (make_obj "element" % elt_to_string) content) ]
    @ make_top_app_bar props
    @ make_side_sheet side_sheet

  let make_icon icon =
    match icon with
    | Some e -> `O ["value", `String (elt_to_string e)]
    | None   -> `Null

  let make_node ~template paths gen_item_list (Item { path; page_gen; _ }) =
    let table = Hashtbl.create 16 in
    (* Check if the same path was added twice *)
    if Uri.Path.Format.has_template paths path
    then failwith (Printf.sprintf "path '%s' is already present"
                   @@ Uri.Path.Format.doc path);
    Uri.Path.Format.store_template paths path;
    let generate =
      (* Use args if memoized templates are different *)
      let gen_template _args user : gen_result =
        match Hashtbl.find_opt table user with
        | Some v -> `HTML v
        | None ->
          match page_gen user with
          | `Template content ->
            let params =
              content
              @ [ "navigation", `A (gen_item_list user)
                ; "username", `String (User.to_string user)
                ; "usericon", `String ""
                ; "usercolor", `String ""
                ]
            in
            let v = Mustache.render template (`O params) in
            Hashtbl.replace table user v;
            `HTML v
          | x -> x
      in Uri.Path.Format.kprint gen_template path
    in
    Uri.Dispatcher.make ~path ~query:Uri.Query.[] generate
    |> Uri.Dispatcher.map_node (fun f ->
        fun user _body _env _state ->
          match f user with
          | `Null -> Lwt.return (`Instant (respond_string "" ()))
          | `HTML s -> Lwt.return (`Instant (respond_string s ()))
          | `Not_allowed -> Lwt.return (`Error "Forbidden")
          | `Template _ -> Lwt.return (`Error "Template"))

  let make_nodes ~template paths gen_item_list item =
    match item with
    | Item { subitems = `Simple; _ } as item ->
      [make_node ~template paths gen_item_list item]
    | Item { subitems = `Subtree x; _ } ->
      List.map (make_node ~template paths gen_item_list) x

  let reference ?(restrict=[]) ?(priority=`None) ~title ~href =
    let not_allowed id = List.exists (User.equal id) restrict in
    let item_gen user =
      if not_allowed user
      then `Not_allowed
      else `Template [ "title",  `String title
                     ; "active", `Bool false
                     ; "href",   `String (Uri.to_string href)
                     ; "simple", `Bool true
                     ]
    in
    let it =
      Item { path = Uri.Path.Format.of_string @@ Uri.to_string href
           ; subitems = `Simple
           ; item_gen
           ; page_gen = fun _ -> `Null
           }
    in [ priority, it ]

  let parametric ?(restrict=[]) ~path props : 'a item list =
    let not_allowed id = List.exists (User.equal id) restrict in
    let page_gen user =
      if not_allowed user
      then `Not_allowed
      else `Template (make_template user props)
    in
    let it =
      Item { path
           ; subitems = `Simple
           ; item_gen = (fun _ -> `Null)
           ; page_gen
           }
    in [ `None, it ]

  let simple ?(restrict=[]) ?(priority=`None) ~title ?icon ~path props =
    let not_allowed id = List.exists (User.equal id) restrict in
    let item_gen user =
      if not_allowed user
      then `Not_allowed
      else `Template [ "title", `String title
                     ; "active", `Bool false
                     ; "icon", (make_icon icon)
                     (* TODO refactor later *)
                     ; "href", `String ("/" ^ Uri.Path.to_string path)
                     ; "simple", `Bool true ]
    in
    let page_gen user =
      if not_allowed user
      then `Not_allowed
      else `Template (make_template user props)
    in
    let it =
      Item { path = Uri.Path.Format.of_string @@ Uri.Path.to_string path
           ; subitems = `Simple
           ; item_gen
           ; page_gen
           }
    in [ priority, it ]

  let make_subitems user lst : Mustache.Json.value list =
    let convert (Item { item_gen; _ }) =
      match item_gen user with
      | `Template x -> Some (`O x)
      | _ -> None in
    filter_map convert lst

  let subtree ?(restrict=[]) ?priority ~title ?icon (list : inner item list) =
    let not_allowed id = List.exists (User.equal id) restrict in
    let priority = match priority with
      | Some v -> v
      | None -> list_max ~default:`None priority_compare (List.map fst list)
    in
    let list =
      List.map snd
      @@ List.sort priority_compare_pair list in
    let item_gen user =
      if not_allowed user
      then `Not_allowed
      else match make_subitems user list with
        | [] -> `Null
        | st -> `Template [ "title", `String title
                          ; "active", `Bool false
                          ; "icon", (make_icon icon)
                          ; "simple", `Bool false
                          ; "subtree", `A st ]
    in
    let it =
      Item { path = Uri.Path.Format.of_string ""
           ; subitems = `Subtree list
           ; item_gen
           ; page_gen = fun _ -> `Null
           }
    in [ priority, it ]

  let make ~template (list : topmost item list) =
    let template = Mustache.of_string template in
    let paths = Uri.Path.Format.templates () in
    let list =
      list
      |> List.sort priority_compare_pair
      |> List.map snd
    in
    let gen_item_list user =
      filter_map (fun (Item { item_gen; _ }) ->
          match item_gen user with
          | `Template x -> Some (`O x)
          | _ -> None) list
    in
    List.flatten
    @@ List.map (fun node ->
        List.map (fun x -> `GET, x)
        @@ make_nodes paths ~template gen_item_list node) list
end
