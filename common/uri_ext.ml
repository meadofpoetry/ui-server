include Uri

let (%) f g x = f (g x)

module Scheme = struct
  type t = string

  let ws    = "ws"
  let wss   = "wss"
  let http  = "http"
  let https = "https"

  let is_ws = function
    | Some "ws" | Some "wss" -> true
    | _                      -> false
end

module Path : sig
  type t = string list
  type templ

  val next : t -> string option * t
  val to_templ : t -> templ
  val templ_compare : templ -> templ -> int
  val of_string : string -> t
  val to_string : t -> string

  module Format : sig
    type _ fmt =
      | String : string fmt
      | Int    : int fmt 
      | Bool   : bool fmt
    type (_,_) t
    val to_templ : (_,_) t -> templ
    val empty : ('a,'a) t
    val (@/) : string -> ('a,'b) t -> ('a,'b) t
    val (^/) : 'a fmt -> ('b,'c) t -> ('a -> 'b,'c) t
    val (/)  : ('a,'b) t -> ('b,'c) t -> ('a,'c) t
    val scan_unsafe : string list -> ('a,'b) t -> 'a -> 'b
    val kprint : (string list -> 'b) -> ('a, 'b) t -> 'a
    val doc : ('a, 'b) t -> string 
  end
end = struct

  type t = string list
         
  type templ = [ `S of string | `Hole ] list

  let split s =
      String.split_on_char '/' s
      |> List.filter (not % String.equal "")
    
  let merge = String.concat "/"

  let of_string = split

  let to_string = merge

  let to_templ = List.map (fun s -> `S s)

  let rec templ_compare l r = match l, r with
    | `S l::ls, `S r::rs -> let c = String.compare l r in
                            if c = 0 then templ_compare ls rs
                            else c
    | _::ls, _::rs -> templ_compare ls rs
    | [], [] -> 0
    | _, [] -> 1
    | [], _ -> -1
    
  let rec next = function
    | [] as l -> None, l
    | h::tl   -> Some h, tl

  module Format = struct
    type _ fmt =
      | String : string fmt
      | Int    : int fmt
      | Bool   : bool fmt
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
      | E -> fun y -> y
      | S (s,x) -> fun y -> S (s, x / y)
      | F (t,x) -> fun y -> F (t, x / y)

    let rec scan_unsafe : type a b. string list -> (a,b) t -> a -> b = fun path fmt f ->
      match path, fmt with
      | _::tl, S (_,fmt) -> scan_unsafe tl fmt f
      | h::tl, F (String, fmt) -> scan_unsafe tl fmt (f h)
      | h::tl, F (Int, fmt) -> scan_unsafe tl fmt (f @@ int_of_string h)
      | h::tl, F (Bool, fmt) -> scan_unsafe tl fmt (f @@ bool_of_string h)
      | [], E -> f
      | _ -> failwith "bad path"

    let rec kprint : type a b. (string list -> b) -> (a, b) t -> a =
      fun k ->
      function
      | E -> k []
      | S (s, rest) -> kprint (fun lst -> k (s :: lst)) rest
      | F (String, rest) -> let f x = kprint (fun lst -> k (x :: lst)) rest in f
      | F (Int, rest) -> let f x = kprint (fun lst -> k ((string_of_int x) :: lst)) rest in f
      | F (Bool, rest) -> let f x = kprint (fun lst -> k ((string_of_bool x) :: lst)) rest in f

    let doc : type a b. (a, b) t -> string = fun fmt ->
      let rec loop : type a b. (a, b) t -> string list = function
        | E -> []
        | S (s,fmt) -> s :: (loop fmt)
        | F (String, fmt) -> ":string" :: (loop fmt)
        | F (Int, fmt) -> ":int" :: (loop fmt)
        | F (Bool, fmt) -> ":bool" :: (loop fmt)
      in
      merge @@ loop fmt
      
  end
end

module Query = struct

  type err = Key_not_found of string
           | Parser_error  of string [@@deriving yojson]
  
  exception Key_not_found_exn of string
  
  type t = (string * string list) list [@@deriving yojson]

  let empty = []

  let err_to_string = function
    | Key_not_found s -> s
    | Parser_error s  -> s

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
    val to_query : t -> string list
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

  module List (E : Show) = struct
    type t = E.t list
    let typ = "list of " ^ E.typ
    let of_query = List.map E.of_string
    let to_query = List.map E.to_string
  end

  module Single (E : Show) = struct
    type t = E.t
    let typ = "mandatory " ^ E.typ
    let of_query = function [v] -> E.of_string v
                          | [] -> raise_notrace Not_found
                          | _ -> raise_notrace (Failure "Single")
    let to_query v = [ E.to_string v ]
  end

  module Option (E : Show) = struct
    type t = E.t option
    let typ = "optional " ^ E.typ
    let of_query = function [] -> None
                          | [v] -> Some (E.of_string v)
                          | _ -> raise_notrace (Failure "Option")
    let to_query = function Some v -> [ E.to_string v ] | None -> []
  end

  type (_,_) compose =
    | (::) : (string * (module Convert with type t = 'a)) * ('b, 'c) compose -> ('a -> 'b, 'c) compose
    | []   : ('c, 'c) compose

  let empty : ('a,'b) compose = []

  let rec make_q : type ty v. (t -> v) -> (ty, v) compose -> ty =
    fun k ->
    function
    | [] -> k []
    | (q, (module C)) :: rest ->
       let f x = make_q (fun lst  -> k ((q, (C.to_query x))::lst)) rest
       in f

  let make_query q = make_q (fun x -> x) q

  let merge t1 t2 = CCList.sorted_merge_uniq ~cmp:(fun x y -> CCString.compare (fst x) (fst y)) t1 t2

  let rec parse_q : type ty v. ty -> (ty, v) compose -> t -> v * t =
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
    |> CCResult.map fst

  let doc queries =
    let rec loop : type a b. (a, b) compose -> (string * string) list = function
      | [] -> CCList.empty
      | (name, (module C)) :: tl ->
         CCList.cons (name, C.typ) (loop tl)
    in
    loop queries

end

type uri =
  { scheme : string option
  ; path   : Path.t
  ; query  : Query.t
  }

let sep u : uri =
  { scheme = scheme u
  ; path   = Path.of_string @@ path u
  ; query  = query u
  }

let upgrade_path s path : uri = { s with path }

let uri_path (s : uri) = s.path

let of_uri t =
  make ?scheme:t.scheme ~path:(Path.to_string t.path) ~query:t.query ()

let handle_uri ~path ~query = fun f uri ->
  let sf = Path.Format.scan_unsafe uri.path path f in
  match Query.parse_query query sf uri.query with
  | Ok r -> r
  | Error _ -> failwith "bad query"

let make_uri ?scheme ~path ~query =
  Path.Format.kprint (fun p -> Query.make_q (fun q -> {scheme; path = p; query = q}) query) path

let construct ?scheme ?host ?port ~path ~query =
  Path.Format.kprint (fun p -> Query.make_q (fun (q:Query.t) ->
                                   let (uri:uri) = { scheme; path = p; query = q } in
                                   of_uri uri
                                   |> (fun u -> with_port u port)
                                   |> (fun u -> with_host u host)) query) path

let kconstruct ?scheme ?host ?port ~f ~path ~query =
  Path.Format.kprint (fun p -> Query.make_q (fun (q:Query.t) ->
                                   let (uri:uri) = { scheme; path = p; query = q } in
                                   of_uri uri
                                   |> (fun u -> with_port u port)
                                   |> (fun u -> with_host u host)
                                   |> f) query) path

module Dispatcher = struct

  module M = Map.Make (struct type t = Path.templ let compare = Path.templ_compare end)

  module P = Path.Format

  module Q = Query

  type 'a node = { docstring : string option
                 ; templ     : Path.templ
                 ; path_typ  : string
                 ; query_typ : (string * string) list
                 ; handler   : uri -> 'a
                 }
  type 'a t = 'a node M.t

  let empty : 'a t = M.empty

  let make ?
           docstring ~path ~query handler =
    let templ = Path.Format.to_templ path in
    let handler uri = handle_uri ~path ~query handler uri in
    let path_typ = Path.Format.doc path in
    let query_typ = Query.doc query in
    { docstring; handler; templ; path_typ; query_typ }

  let add (m : 'a t) (node : 'a node) =
    M.add node.templ node m

  let dispatch (m : 'a t) uri =
    let templ = Path.to_templ uri.path in
    (M.find templ m).handler uri

  let doc (m : 'a t) : string list =
    let gen node =
      let queries q = String.concat "" @@ List.map (fun (name, typ) -> Printf.sprintf "\t%s : %s\n" name typ) q in
      let doc = match node.docstring with None -> "Absent" | Some s -> s in
      Printf.sprintf "\t%s\nDoc: %s\n%s" node.path_typ doc (queries node.query_typ)
    in M.fold (fun _ node acc -> (gen node) :: acc) m []
    
end  
