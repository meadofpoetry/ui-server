include module type of struct
    include Lwt_react
  end
  with module E := Lwt_react.E
  with module S := Lwt_react.S

type step = React.step

module E : sig
  include module type of Lwt_react.E

  val changes : eq:('a -> 'a -> bool) -> 'a event -> 'a event

  val aggregate_merge :
    merge:('a list -> 'b -> 'a list) ->
    (unit -> unit Lwt.t) ->
    'b t list ->
    'a list t

  val aggregate : (unit -> unit Lwt.t) -> 'a t list -> 'a list t
end

module S : sig
  include module type of Lwt_react.S

  val create :
    eq:('a -> 'a -> bool) -> 'a -> 'a signal * (?step:step -> 'a -> unit)

  val equal : eq:('a -> 'a -> bool) -> 'a signal -> 'a signal -> bool

  val hold : eq:('a -> 'a -> bool) -> 'a -> 'a event -> 'a signal

  val app : eq:('b -> 'b -> bool) -> ('a -> 'b) signal -> 'a signal -> 'b signal

  val map : eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a signal -> 'b signal

  val filter :
    eq:('a -> 'a -> bool) -> ('a -> bool) -> 'a -> 'a signal -> 'a signal

  val fmap :
    eq:('b -> 'b -> bool) -> ('a -> 'b option) -> 'b -> 'a signal -> 'b signal

  val on : eq:('a -> 'a -> bool) -> bool signal -> 'a -> 'a signal -> 'a signal

  val when_ :
    eq:('a -> 'a -> bool) -> bool signal -> 'a -> 'a signal -> 'a signal

  val dismiss :
    eq:('a -> 'a -> bool) -> 'b event -> 'a -> 'a signal -> 'a signal

  val accum : eq:('a -> 'a -> bool) -> ('a -> 'a) event -> 'a -> 'a signal

  val fold :
    eq:('a -> 'a -> bool) -> ('a -> 'b -> 'a) -> 'a -> 'b event -> 'a signal

  val merge :
    eq:('a -> 'a -> bool) ->
    ('a -> 'b -> 'a) ->
    'a ->
    'b signal list ->
    'a signal

  val switch : eq:('a -> 'a -> bool) -> 'a signal signal -> 'a signal

  val fix : eq:('a -> 'a -> bool) -> 'a -> ('a signal -> 'a signal * 'b) -> 'b

  val l1 : eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a signal -> 'b signal

  val l2 :
    eq:('c -> 'c -> bool) ->
    ('a -> 'b -> 'c) ->
    'a signal ->
    'b signal ->
    'c signal

  val l3 :
    eq:('d -> 'd -> bool) ->
    ('a -> 'b -> 'c -> 'd) ->
    'a signal ->
    'b signal ->
    'c signal ->
    'd signal

  val l4 :
    eq:('e -> 'e -> bool) ->
    ('a -> 'b -> 'c -> 'd -> 'e) ->
    'a signal ->
    'b signal ->
    'c signal ->
    'd signal ->
    'e signal

  val l5 :
    eq:('f -> 'f -> bool) ->
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
    'a signal ->
    'b signal ->
    'c signal ->
    'd signal ->
    'e signal ->
    'f signal

  val l6 :
    eq:('g -> 'g -> bool) ->
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
    'a signal ->
    'b signal ->
    'c signal ->
    'd signal ->
    'e signal ->
    'f signal ->
    'g signal

  (* Lwt react *)

  val bind :
    eq:('b -> 'b -> bool) -> 'a signal -> ('a -> 'b signal) -> 'b signal

  val bind_s :
    eq:('b -> 'b -> bool) ->
    'a signal ->
    ('a -> 'b signal Lwt.t) ->
    'b signal Lwt.t

  val limit :
    eq:('a -> 'a -> bool) -> (unit -> unit Lwt.t) -> 'a signal -> 'a signal

  val app_s :
    eq:('b -> 'b -> bool) ->
    ('a -> 'b Lwt.t) signal ->
    'a signal ->
    'b signal Lwt.t

  val map_s :
    eq:('b -> 'b -> bool) -> ('a -> 'b Lwt.t) -> 'a signal -> 'b signal Lwt.t

  val filter_s :
    eq:('a -> 'a -> bool) ->
    ('a -> bool Lwt.t) ->
    'a ->
    'a signal ->
    'a signal Lwt.t

  val fmap_s :
    eq:('b -> 'b -> bool) ->
    ('a -> 'b option Lwt.t) ->
    'b ->
    'a signal ->
    'b signal Lwt.t

  val accum_s :
    eq:('a -> 'a -> bool) -> ('a -> 'a Lwt.t) event -> 'a -> 'a signal

  val fold_s :
    eq:('a -> 'a -> bool) ->
    ('a -> 'b -> 'a Lwt.t) ->
    'a ->
    'b event ->
    'a signal

  val merge_s :
    eq:('a -> 'a -> bool) ->
    ('a -> 'b -> 'a Lwt.t) ->
    'a ->
    'b signal list ->
    'a signal Lwt.t

  val l1_s :
    eq:('b -> 'b -> bool) -> ('a -> 'b Lwt.t) -> 'a signal -> 'b signal Lwt.t

  val l2_s :
    eq:('c -> 'c -> bool) ->
    ('a -> 'b -> 'c Lwt.t) ->
    'a signal ->
    'b signal ->
    'c signal Lwt.t

  val l3_s :
    eq:('d -> 'd -> bool) ->
    ('a -> 'b -> 'c -> 'd Lwt.t) ->
    'a signal ->
    'b signal ->
    'c signal ->
    'd signal Lwt.t

  val l4_s :
    eq:('e -> 'e -> bool) ->
    ('a -> 'b -> 'c -> 'd -> 'e Lwt.t) ->
    'a signal ->
    'b signal ->
    'c signal ->
    'd signal ->
    'e signal Lwt.t

  val l5_s :
    eq:('f -> 'f -> bool) ->
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f Lwt.t) ->
    'a signal ->
    'b signal ->
    'c signal ->
    'd signal ->
    'e signal ->
    'f signal Lwt.t

  val l6_s :
    eq:('g -> 'g -> bool) ->
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g Lwt.t) ->
    'a signal ->
    'b signal ->
    'c signal ->
    'd signal ->
    'e signal ->
    'f signal ->
    'g signal Lwt.t

  val run_s : eq:('a -> 'a -> bool) -> 'a Lwt.t signal -> 'a signal Lwt.t
end
