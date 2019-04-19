include Uri

let (%) f g x = f (g x)

let get_exn = function None -> raise Not_found | Some x -> x

let cons_maybe = function
  | None -> fun l -> l
  | Some v -> fun l -> v::l

let opt_map f = function
  | None -> None
  | Some v -> Some (f v)

let result_map f = function
  | Error _ as e -> e
  | Ok v -> Ok (f v)

module Scheme = struct
  type uri = t
  type t = string

  let ws    = "ws"
  let wss   = "wss"
  let http  = "http"
  let https = "https"
  let udp   = "udp"

  let ws_pat = [ws; wss]
  let http_pat = [http; https]

  let is uri s =
    match scheme uri with
    | None -> false
    | Some scheme -> String.equal scheme s
end

module Path = struct

  type uri = t

  type t = string list
         
  type templ = [ `S of string | `Hole ] list

  let empty = []

  let rec equal l r =
    match l, r with
    | [], [] -> true
    | [], _ | _, [] -> false
    | xl::l', xr::r' -> String.equal xl xr && equal l' r'

  let split s =
      String.split_on_char '/' s
      |> List.filter (not % String.equal "")
    
  let merge = String.concat "/"

  let of_string = split

  let to_string = merge

  let of_uri uri = of_string @@ path uri
                
  let to_templ = List.map (fun s -> `S s)

  let templ_to_string t =
    let rec loop acc = function
      | [] -> merge @@ List.rev acc
      | `Hole::tl -> loop (":param:"::acc) tl
      | `S s::tl -> loop (s::acc) tl
    in loop [] t
      
  let concat = (@)

  let rec templ_compare l r = match l, r with
    | `S l::ls, `S r::rs -> let c = String.compare l r in
                            if c = 0 then templ_compare ls rs
                            else c
    | _::ls, _::rs -> templ_compare ls rs
    | [], [] -> 0
    | _, [] -> 1
    | [], _ -> -1
    
  let next = function
    | [] as l -> None, l
    | h::tl   -> Some h, tl

  module Format = struct
    type _ fmt =
      | String : string fmt
      | Int    : int fmt
      | Int32  : int32 fmt
      | Uuid   : Uuidm.t fmt
      | Bool   : bool fmt
      | Any    : unit fmt
    type (_,_) t =
      | S : string * ('a,'b) t -> ('a,'b) t
      | F : 'a fmt * ('b,'c) t -> ('a -> 'b, 'c) t
      | E : ('a,'a) t

    let rec to_templ : type a b. (a,b) t -> templ = function
      | E -> []
      | F (_,fmt) -> `Hole :: (to_templ fmt)
      | S (s,fmt) -> `S s :: (to_templ fmt)
      
    let empty = E

    let (@/) s fmt =
      let toks = split s in
      let rec merge = function
        | [] -> fmt
        | h::tl -> S (h, merge tl)
      in merge toks

    let (^/) f fmt = F (f,fmt)

    let rec (/) : type a b c. (a,b) t -> (b,c) t -> (a,c) t = function
      | E       -> fun y -> y
      | S (s,x) -> fun y -> S (s, x / y)
      | F (t,x) -> fun y -> F (t, x / y)

    let rec scan_unsafe : type a b. string list -> (a,b) t -> a -> b = fun path fmt f ->
      match path, fmt with
      | _ :: tl, S (_, fmt) -> scan_unsafe tl fmt f
      | h :: tl, F (String, fmt) -> scan_unsafe tl fmt (f h)
      | h :: tl, F (Int, fmt) -> scan_unsafe tl fmt (f @@ int_of_string h)
      | h :: tl, F (Int32, fmt) -> scan_unsafe tl fmt (f @@ Int32.of_string h)
      | h :: tl, F (Uuid, fmt) -> scan_unsafe tl fmt (f @@ get_exn @@ Uuidm.of_string h)
      | h :: tl, F (Bool, fmt) -> scan_unsafe tl fmt (f @@ bool_of_string h)
      | _ :: tl, F (Any, fmt) -> scan_unsafe tl fmt (f ()) 
      | [], E -> f
      | t, fmt ->
        let t = String.concat "/" t in
        let s = templ_to_string @@ to_templ fmt in
        failwith @@ Printf.sprintf "path: %s, fmt: %s" t s
      | exception _ -> failwith "bad path"
                       
    let rec kprint : type a b. (string list -> b) -> (a, b) t -> a =
      fun k ->
      function
      | E                -> k []
      | S (s, rest)      -> kprint (fun lst -> k (s :: lst)) rest
      | F (String, rest) -> let f x = kprint (fun lst -> k (x :: lst)) rest in f
      | F (Int, rest)    -> let f x = kprint (fun lst -> k ((string_of_int x) :: lst)) rest in f
      | F (Int32, rest)  -> let f x = kprint (fun lst -> k ((Int32.to_string x) :: lst)) rest in f
      | F (Uuid, rest)   -> let f x = kprint (fun lst -> k ((Uuidm.to_string x) :: lst)) rest in f
      | F (Bool, rest)   -> let f x = kprint (fun lst -> k ((string_of_bool x) :: lst)) rest in f
      | F (Any, rest) -> let f () = kprint (fun lst -> k ("*" :: lst)) rest in f

    let doc : type a b. (a, b) t -> string = fun fmt ->
      let rec loop : type a b. (a, b) t -> string list = function
        | E               -> []
        | S (s,fmt)       -> s :: (loop fmt)
        | F (String, fmt) -> ":string" :: (loop fmt)
        | F (Int, fmt)    -> ":int" :: (loop fmt)
        | F (Int32, fmt)  -> ":int32" :: (loop fmt)
        | F (Uuid, fmt)   -> ":uuid"  :: (loop fmt)
        | F (Bool, fmt)   -> ":bool" :: (loop fmt)
        | F (Any, fmt)    -> ":any" :: (loop fmt)
      in
      merge @@ loop fmt

    let of_string x = x @/ empty

    type paths = templ list ref

    let templates () = ref []

    let store_template paths fmt =
      let templ = to_templ fmt in
      paths := templ::(!paths)

    let has_template paths fmt =
      let templ = to_templ fmt in
      List.exists (fun x -> templ_compare templ x = 0) !paths
(*
    let result_conv (type a b c d) (path : (a,b) t) (path2 : (c,d) t) (f : a) (conv : b -> d) : c =
      let rec loop : type a b c d. a -> (b -> d) -> (a, b) t -> (c, d) t -> c =
        fun k c l r -> match l, r with
                       | E, E -> c k
                       | S (_,fmtl), S (_,fmtr) -> loop k c fmtl fmtr
                       | F (String, fmtl), F (String, fmtr) -> fun x -> loop (k x) c fmtl fmtr
                       | F (Int, fmtl), F (Int, fmtr)    -> fun x -> loop (k x) c fmtl fmtr
                       | F (Int32, fmtl), F (Int32, fmtr)  -> fun x -> loop (k x) c fmtl fmtr
                       | F (Uuid, fmtl), F (Uuid, fmtr)   -> fun x -> loop (k x) c fmtl fmtr
                       | F (Bool, fmtl), F (Bool, fmtr)   -> fun x -> loop (k x) c fmtl fmtr
                       | F (Any, fmtl), F (Any, fmtr)    -> fun x -> loop (k x) c fmtl fmtr
                       | _ -> assert false
      in loop f conv path path2
 *)
  end
end

module Query = struct

  type err = Key_not_found of string
           | Parser_error  of string
  
  exception Key_not_found_exn of string
  
  type t = (string * string list) list
  (*
  let err_to_string = function
    | Key_not_found s -> s
    | Parser_error s  -> s
   *)
  let grep_arg (name : string) lst =
    let rec grep' acc = function
      | [] -> [], lst
      | (title, arg)::tl ->
         if String.equal title name
         then (arg, (List.rev acc) @ tl)
         else grep' ((title, arg)::acc) tl
    in grep' [] lst
   
  module type Show = sig
    type t
    val typ : string
    val to_string : t -> string
    val of_string : string -> t 
  end

  module type Convert = sig
    type t
    val typ : string
    val to_query : t -> string list option
    val of_query : string list -> t
  end

  module String = struct
    type t = string
    let typ = "string"
    let of_string x = x
    let to_string x = x
  end

  module Int = struct
    type t = int
    let typ = "int"
    let of_string = int_of_string
    let to_string = string_of_int
  end

  module Float = struct
    type t = float
    let typ = "float"
    let of_string = float_of_string
    let to_string = string_of_float
  end

  module Int32 = struct include Int32 let typ = "int32" end
  module Int64 = struct include Int64 let typ = "int64" end

  module Bool = struct
    type t = bool
    let typ = "bool"
    let of_string = bool_of_string
    let to_string = string_of_bool
  end

  module Either (L:Show)(R:Show) = struct
    type t = [ `Left of L.t | `Right of R.t ]
    let typ = L.typ ^ " or " ^ R.typ
    let of_string s = try `Left (L.of_string s)
                      with _ -> try `Right (R.of_string s)
                                with _ -> raise_notrace (Failure "Neither")
    let to_string = function
      | `Left x  -> L.to_string x
      | `Right x -> R.to_string x
  end

  module List (E : Show) : Convert with type t = E.t list = struct
    type t = E.t list
    let typ = "list of " ^ E.typ
    let of_query = List.map E.of_string
    let to_query v = match v with
      | [] -> None
      | v  -> Some (List.map E.to_string v)
  end

  module Single (E : Show) : Convert with type t = E.t = struct
    type t = E.t
    let typ = "mandatory " ^ E.typ
    let of_query = function [v] -> E.of_string v
                          | [] -> raise_notrace Not_found
                          | _ -> raise_notrace (Failure "Single")
    let to_query v = Some [ E.to_string v ]
  end

  module Option (E : Show) : Convert with type t = E.t option = struct
    type t = E.t option
    let typ = "optional " ^ E.typ
    let of_query = function [] -> None
                          | [v] -> Some (E.of_string v)
                          | _ -> raise_notrace (Failure "Option")
    let to_query = function Some v -> Some [ E.to_string v ] | None -> None
  end

  type (_,_) format =
    | (::) : (string * (module Convert with type t = 'a)) * ('b, 'c) format
             -> ('a -> 'b, 'c) format
    | []   : ('c, 'c) format

  let empty : (_,_) format = []

  let rec make_q : type ty v. (t -> v) -> (ty, v) format -> ty =
    fun k ->
    function
    | [] -> k []
    | (q, (module C)) :: rest ->
       let f x = make_q (fun lst  ->
                     k (cons_maybe (opt_map (fun x -> q,x) (C.to_query x)) lst)) rest
       in f

  let make q = make_q (fun x -> x) q

  let concat = Stdlib.List.append
   (*
  let concat t1 t2 = CCList.sorted_merge_uniq
                       ~cmp:(fun x y -> String.compare (fst x) (fst y)) t1 t2
   *)
  let rec parse_q : type ty v. ty -> (ty, v) format -> t -> v * t =
    fun k ->
    function
    | [] ->
       fun rest -> k,rest
    | (q, (module C)) :: rest ->
       fun sl ->
       let (arg, args) = grep_arg q sl in
       parse_q (k (try C.of_query arg
                   with Not_found -> raise_notrace (Key_not_found_exn q)
                      | exn -> raise_notrace exn)) rest args

  let parse_query' lst f queries =
    try Ok(parse_q f lst queries)
    with Key_not_found_exn key -> Error (Key_not_found key)
       | exn                   -> Error (Parser_error (Printexc.to_string exn))

  let parse_query lst f queries =
    parse_query' lst f queries
    |> result_map fst

  [@@@ocaml.warning "-33"]
  let doc queries =
    let rec loop : type a b. (a, b) format -> (string * string) list = function
      | [] -> Stdlib.List.[]
      | (name, (module C)) :: tl ->
         Stdlib.List.cons (name, C.typ) (loop tl)
    in
    loop queries

end
             
let handle_uri ~path ~query = fun f uri ->
  let uri_path = Path.of_string @@ Uri.path uri in
  let uri_query = Uri.query uri in
  let sf = Path.Format.scan_unsafe uri_path path f in
  match Query.parse_query query sf uri_query with
  | Ok r -> r
  | Error _ -> failwith "bad query"

module Dispatcher = struct

  type uri = t
                  
  module M = Map.Make (struct
                 type t = Path.templ
                 let compare = Path.templ_compare
               end)

  module P = Path.Format

  module Q = Query

  type 'a node = { docstring : string option
                 ; templ     : Path.templ
                 ; path_typ  : string
                 ; query_typ : (string * string) list
                 ; handler   : uri -> 'a
                 }
               
  type 'a t = 'a node M.t

  let prepend prefix node =
    let templ = (Path.to_templ prefix) @ node.templ in
    (* FIXME dirty hack *)
    let handler uri =
      let suffix =
        Uri.with_path uri
        @@ Path.merge
        @@ List.tl
        @@ Path.of_string
        @@ Uri.path uri in
      node.handler suffix in
    { node with templ; handler }

  let empty : 'a t = M.empty

  let make ?docstring  ~path ~query handler =
    let templ = Path.Format.to_templ path in
    let handler uri = handle_uri ~path ~query handler uri in
    let path_typ = Path.Format.doc path in
    let query_typ = Query.doc query in
    { docstring; handler; templ; path_typ; query_typ }

  let map_node f node =
    { node with handler = (fun uri -> f @@ node.handler uri) }

  exception Ambiguity of string
    
  let add (m : 'a t) (node : 'a node) =
    try
      let _ = M.find node.templ m in
      raise_notrace (Ambiguity (Path.templ_to_string node.templ))
    with Not_found ->
      M.add node.templ node m

  let merge m lst =
    let update_fun templ new_v old =
      match old with
      | None -> Some new_v
      | _ -> raise_notrace (Ambiguity (Path.templ_to_string templ))
    in
    List.fold_left (fun m (prefix, disp_list) ->
        let prefix_templ = Path.to_templ prefix in
        List.fold_left (fun m disp ->
            M.fold (fun path node m ->
                let templ = prefix_templ @ path in
                (* FIXME rewrite *)
                let handler uri =
                  let len = List.length prefix_templ in
                  let rec drop n l = match l with
                    | [] -> []
                    | _ when n=0 -> l
                    | _::l' -> drop (n-1) l' in
                  let path' =
                    Path.merge
                    @@ drop len
                    @@ Path.of_string
                    @@ Uri.path uri in
                  let suffix = Uri.with_path uri path' in
                  node.handler suffix in
                M.update templ (update_fun templ { node with templ; handler }) m)
              disp m)
          m disp_list)
      m lst
(*
  let merge_unsafe lst =
    let merge_fun _templ l r =
      match l, r with
      | None, (Some _ as v)
        | (Some _ as v), None -> v
      | _ -> failwith "Netlib.Uri.Dispatcher.merge_unsafe: key exists in several dispatchers"
    in
    List.fold_left (fun m disp ->
        M.merge merge_fun m disp) M.empty lst
 *)
    
  let concat (l : 'a t list) =
    let merge_fun templ l r =
      match l, r with
      | None, (Some _ as v)
        | (Some _ as v), None -> v
      | _ -> raise_notrace (Ambiguity (Path.templ_to_string templ))
    in
    match l with
    | [] -> M.empty
    | [x] -> x
    | acc::l' ->
       List.fold_left (M.merge merge_fun) acc l'

  let dispatch ~default (m : 'a t) (uri : uri) =
    let templ  = Path.to_templ (Path.of_string @@ path uri) in
    try (M.find templ m).handler uri
    with exn -> print_endline @@ Printexc.to_string exn; default

  let doc (m : 'a t) : string list =
    let gen node =
      let queries q = String.concat "" @@ List.map (fun (name, typ) -> Printf.sprintf "\t%s : %s\n" name typ) q in
      let doc = match node.docstring with None -> "Absent" | Some s -> s in
      Printf.sprintf "\t%s\nDoc: %s\n%s" (Path.templ_to_string node.templ) doc (queries node.query_typ)
    in M.fold (fun _ node acc -> (gen node) :: acc) m []

end

let typ = "uri"

let to_yojson x = `String (Uri.to_string x)

let of_yojson = function
  | `String s -> Ok (Uri.of_string s)
  | _ -> Error "Netlib.Uri.of_yojson"
                  
let path_v4 uri =
  try Some (Ipaddr_ext.V4.of_string_exn @@ path uri)
  with _ -> None

let with_path_v4 uri ip =
  with_path uri @@ Ipaddr_ext.V4.to_string ip

let with_path_parsed uri p =
  with_path uri @@ Path.to_string p
                  
let construct ?scheme ?host ?port ~path ~query =
  Path.Format.kprint (fun p ->
      Query.make_q (fun (query:Query.t) ->
          let path = Path.to_string p in
          make ?scheme ?host ?port ~path ~query ())
        query)
    path

let kconstruct ?scheme ?host ?port ~f ~path ~query =
  Path.Format.kprint (fun p ->
      Query.make_q (fun (query:Query.t) ->
          let path = Path.to_string p in
          f @@ make ?scheme ?host ?port ~path ~query ())
        query)
    path
