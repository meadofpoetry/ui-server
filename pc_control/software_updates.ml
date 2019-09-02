type error = [ `PackageKit of string ]

let pp_error ppf = function
  | `PackageKit s -> Fmt.fmt "PackageKit: %s" ppf s

type version = string

type t = { pk : Packagekit.t
         ; updated : version -> unit Lwt.t
         ; current : version
         }
             
let create version ?(updated = fun (_ : version) -> Lwt.return_unit) () =
  let ( let* ) = Lwt_result.bind in
  let* pk = Packagekit.create () in
  Lwt_result.return { pk; current = version; updated }
