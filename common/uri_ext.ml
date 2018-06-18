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
  val of_string : string -> t
  val to_string : t -> string
  val s : string -> t
  val next : t -> string option * t
  val append : t -> t -> t
  val push   : t -> string -> t
  val (/) : t -> string -> t
end = struct
  type t = string list
  (*      
  let root x =
    let s = CCString.drop_while ((=) '/') x in
    let ind = CCString.find ~sub:"/" s in
    if ind < 0 then (None, x)
    else let (root, rest) = CCString.take_drop ind s in
         (Some root, rest)
   *)
         
  let split s =
    String.split_on_char '/' s
    |> List.filter (not % String.equal "")

  let merge = String.concat "/"

  let of_string = split

  let to_string = merge

  let s = of_string

  let rec next = function
    | [] as l -> None, l
    | h::tl   -> Some h, tl

  let append = List.append

  let push t x = append t @@ split x

  let (/) = push

end

module Query = struct

  type err = Key_not_found of string
           | Parser_error  of string [@@deriving yojson]
  
  exception Key_not_found_exn of string
  
  type t = (string * string list) list [@@deriving yojson]

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
    val to_string : t -> string
    val of_string : string -> t 
  end

  module type Convert = sig
    type t
    val to_query : t -> string list
    val of_query : string list -> t 
  end

  module String = struct
    type t = string
    let of_string x = x
    let to_string x = x
  end

  module Int = struct
    type t = int
    let of_string = int_of_string
    let to_string = string_of_int
  end

  module Bool = struct
    type t = bool
    let of_string = bool_of_string
    let to_string = string_of_bool
  end

  module Either (L:Show)(R:Show) = struct
    type t = [ `Left of L.t | `Right of R.t ]
    let of_string s = try `Left (L.of_string s)
                      with _ -> try `Right (R.of_string s)
                                with _ -> raise_notrace (Failure "Neither")
    let to_string = function
      | `Left x  -> L.to_string x
      | `Right x -> R.to_string x
  end

  module List (E : Show) = struct
    type t = E.t list
    let of_query = List.map E.of_string
    let to_query = List.map E.to_string
  end

  module Single (E : Show) = struct
    type t = E.t
    let of_query = function [v] -> E.of_string v
                          | [] -> raise_notrace Not_found
                          | _ -> raise_notrace (Failure "Single")
    let to_query v = [ E.to_string v ]
  end

  module Option (E : Show) = struct
    type t = E.t option
    let of_query = function [] -> None
                          | [v] -> Some (E.of_string v)
                          | _ -> raise_notrace (Failure "Option")
    let to_query = function Some v -> [ E.to_string v ] | None -> []
  end

  type (_,_) compose =
    | (::) : (string * (module Convert with type t = 'a)) * ('b, 'c) compose -> ('a -> 'b, 'c) compose
    | []   : ('c, 'c) compose

  let rec make_q : type ty v. (t -> v) -> (ty, v) compose -> ty =
    fun k ->
    function
    | [] -> k []
    | (q, (module C)) :: rest ->
       let f x = make_q (fun lst  -> k ((q, (C.to_query x))::lst)) rest
       in f

  let make_query q = make_q (fun x -> x) q

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

end

type sep =
  { scheme : string option
  ; path   : Path.t
  ; query  : Query.t
  }

let sep u : sep =
  { scheme = scheme u
  ; path   = Path.of_string @@ path u
  ; query  = query u
  }

let upgrade_path s path : sep = { s with path }

let sep_path (s : sep) = s.path

(** TO DO remove *)
let split s =
  String.split_on_char '/' s
  |> List.filter (not % String.equal "")
