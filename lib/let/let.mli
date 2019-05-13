
module type Monad = sig
  type 'a t
  val  bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type Monad2 = sig
  type ('a, 'b) t
  val  bind : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t
end
                  
module type Functor = sig
  type 'a t
  val  map : ('a -> 'b) -> 'a t -> 'b t
end

module type Functor2 = sig
  type ('a, 'b) t
  val  map : ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
end

module type Applicative = sig
  type 'a t
  val  apply : ('a -> 'b) t -> 'a t -> 'b t
end

module M (X : Monad) : sig
  val ( let* ) : 'a X.t -> ('a -> 'b X.t) -> 'b X.t
end

module M2 (X : Monad2) : sig
  val ( let* ) : ('a, 'c) X.t -> ('a -> ('b, 'c) X.t) -> ('b, 'c) X.t
end

module F (X : Functor) : sig
  val ( let+ ) : 'a X.t -> ('a -> 'b) -> 'b X.t
end

module F2 (X : Functor2) : sig
  val ( let+ ) : ('a, 'c) X.t -> ('a -> 'b) -> ('b, 'c) X.t
end

module A (X : Applicative) : sig
  val ( let+ ) : 'a X.t -> ('a -> 'b) X.t -> 'b X.t
end
