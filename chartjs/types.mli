module Color : sig
  type t = string [@@deriving show]
  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t

end

module Font : sig
  type family = string [@@deriving show]
  val family_to_js : family -> Ojs.t
  val family_of_js : Ojs.t -> family

  type style = string [@@deriving show]
  val style_to_js : style -> Ojs.t
  val style_of_js : Ojs.t -> style

end

type line_cap =
  [ `Butt [@js "butt"]
  | `Round [@js "round"]
  | `Square [@js "square"]
  ] [@js.enum]
val line_cap_to_js : line_cap -> Ojs.t
val line_cap_of_js : Ojs.t -> line_cap

type line_join =
  [ `Round [@js "round"]
  | `Bevel [@js "bevel"]
  | `Miter [@js "miter"]
  ] [@js.enum]
val line_join_to_js : line_join -> Ojs.t
val line_join_of_js : Ojs.t -> line_join

type line_height = float
val line_height_to_js : line_height -> Ojs.t
val line_height_of_js : Ojs.t -> line_height

type text = string [@@deriving show]
val text_to_js : text -> Ojs.t
  [@@js.custom
   let text_to_js (s : text) : Ojs.t =
     match String.split_on_char '\n' s with
     | [s] -> Ojs.string_to_js s
     | l -> Ojs.list_to_js Ojs.string_to_js l
  ]
val text_of_js : Ojs.t -> text
  [@@js.custom
   let text_of_js (js : Ojs.t) : text =
     match Ojs.obj_type js with
     | "[object Array]" ->
        let l = Ojs.list_of_js Ojs.string_of_js js in
        String.concat "\n" l
     | "[object String]" ->
        Ojs.string_of_js js
     | _ -> assert false
  ]
