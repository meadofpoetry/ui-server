let not_responding () = Lwt.return_error Request.Not_responding

let ( >>= ) = Lwt.bind

let ( % ) f g x = f (g x)

let return_error e = Lwt.return (`Error (Request.error_to_string e))

let return_value v = Lwt.return (`Value v)

let ( >>=? ) x f =
  x
  >>= function
  | Error e -> return_error e
  | Ok x -> f x
