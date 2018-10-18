type t = [`Root | `Operator | `Guest ] [@@deriving eq]

let of_string = function
  | "root"     -> Ok `Root
  | "operator" -> Ok `Operator
  | "guest"    -> Ok `Guest
  | _ -> Error "Unknown user"

let to_string = function
  | `Root     -> "root"
  | `Operator -> "operator"
  | `Guest    -> "guest"
       
let of_yojson = function
  | `String s -> of_string s
  | err -> Error ("typ_of_yojson: wrong data" ^ (Yojson.Safe.to_string err))

let to_yojson u = `String (to_string u)

type pass =
  { user     : t
  ; password : string
  } [@@deriving yojson]

type pass_change =
  { user     : t
  ; old_pass : string
  ; new_pass : string
  } [@@deriving yojson, eq]
          
let to_int = function
  | `Root     -> 0
  | `Operator -> 1
  | `Guest    -> 2

let of_int = function
  | 0 -> `Root
  | 1 -> `Operator
  | _ -> `Guest
          
let is_root = function
  | `Root -> true
  | _    -> false

type 'a user_table =
  { root     : 'a
  ; operator : 'a
  ; guest    : 'a
  }

let empty_table = { root = []
                  ; operator = []
                  ; guest = []
                  }
  
let map_table f tbl =
  { root     = f `Root tbl.root
  ; operator = f `Operator tbl.operator
  ; guest    = f `Guest tbl.guest
  }

let fold_table f acc tbl =
  f acc `Root tbl.root
  |> (fun acc -> f acc `Operator tbl.operator)
  |> (fun acc -> f acc `Guest tbl.guest)

let concat_table (tbls : 'a list user_table list) : 'a list user_table =
  let init = { root     = []
             ; operator = []
             ; guest    = []
             } in
  List.fold_left (fun acc tbl ->
      { root     = acc.root @ tbl.root
      ; operator = acc.operator @ tbl.operator
      ; guest    = acc.guest @ tbl.guest
    } )
    init tbls
