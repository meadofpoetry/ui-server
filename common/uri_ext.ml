include Uri

let (%) f g x = f (g x)
      
module Path = struct
  type t = string list

  let split : string -> t = fun s ->
    String.split_on_char '/' s
    |> List.filter (not % String.equal "")
end
            
module Query = struct

  type t = (string * string list) list

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

  module List (E : Show) = struct
    type t = E.t list
    let of_query = List.map E.of_string
    let to_query = List.map E.to_string
  end

  module Single (E : Show) = struct
    type t = E.t
    let of_query = function [v] -> E.of_string v
    let to_query v = [ E.to_string v ]
  end

  module Option (E : Show) = struct
    type t = E.t option
    let of_query = function [] -> None | [v] -> Some (E.of_string v)
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
                   
  let rec parse_q : type ty v. ty -> (ty, v) compose -> t -> v =
    fun k ->
    function
    | [] ->
       fun sl -> k
    | (q, (module C)) :: rest ->
       fun sl ->
       let (arg, args) = grep_arg q sl in
       parse_q (k (C.of_query arg)) rest args

  let parse_query lst f queries =
    parse_q f lst queries
    
end

type sep = string option * Path.t * Query.t
      
let sep u : sep =
  (scheme u), (Path.split @@ path u), (query u)

let upgrade_path (sc,_,q) path : sep = (sc,path,q)
  
let sep_path ((_,path,_) : sep) = path
