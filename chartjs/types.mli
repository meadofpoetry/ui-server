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

type easing =
  [ `Linear [@js "linear"]
  | `Ease_in_quad [@js "easeInQuad"]
  | `Ease_out_quad [@js "easeOutQuad"]
  | `Ease_in_out_quad [@js "easeInOutQuad"]
  | `Ease_in_cubic [@js "easeInCubic"]
  | `Ease_out_cubic [@js "easeOutCubic"]
  | `Ease_in_out_cubic [@js "easeInOutCubic"]
  | `Ease_in_quart [@js "easeInQuart"]
  | `Ease_out_quart [@js "easeOutQuart"]
  | `Ease_in_out_quart [@js "easeInOutQuart"]
  | `Ease_in_quint [@js "easeInQuint"]
  | `Ease_out_quint [@js "easeOutQuint"]
  | `Ease_in_out_quint [@js "easeInOutQuint"]
  | `Ease_in_sine [@js "easeInSine"]
  | `Ease_out_sine [@js "easeOutSine"]
  | `Ease_in_out_sine [@js "easeInOutSine"]
  | `Ease_in_expo [@js "easeInExpo"]
  | `Ease_out_expo [@js "easeOutExpo"]
  | `Ease_in_out_expo [@js "easeInOutExpo"]
  | `Ease_in_circ [@js "easeInCirc"]
  | `Ease_out_circ [@js "easeOutCirc"]
  | `Ease_in_out_circ [@js "easeInOutCirc"]
  | `Ease_in_elastic [@js "easeInElastic"]
  | `Ease_out_elastic [@js "easeOutElastic"]
  | `Ease_in_out_elastic [@js "easeInOutElastic"]
  | `Ease_in_back [@js "easeInBack"]
  | `Ease_out_back [@js "easeOutBack"]
  | `Ease_in_out_back [@js "easeInOutBack"]
  | `Ease_in_bounce [@js "easeInBounce"]
  | `Ease_out_bounce [@js "easeOutBounce"]
  | `Ease_in_out_bounce [@js "easeInOutBounce"]
  ] [@js.enum]
val easing_to_js : easing -> Ojs.t
val easing_of_js : Ojs.t -> easing

module Update_config : sig
  type t

  val duration : t -> int
  val set_duration : t -> int -> unit

  val lazy_ : t -> bool
  val set_lazy_ : t -> bool -> unit

  val easing : t -> easing
  val set_easing : t -> easing -> unit

  val make : ?duration:int ->
             ?lazy_:bool ->
             ?easing:easing ->
             unit ->
             t [@@js.builder]

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
