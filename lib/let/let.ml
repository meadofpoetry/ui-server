
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

module M (X : Monad) = struct
  let ( let* ) = X.bind
end

module M2 (X : Monad2) = struct
  let ( let* ) = X.bind
end

module F (X : Functor) = struct
  let ( let+ ) x f = X.map f x
end

module F2 (X : Functor2) = struct
  let ( let+ ) x f = X.map f x
end

module A (X : Applicative) = struct
  let ( let+ ) x f = X.apply f x
end
