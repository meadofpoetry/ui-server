
type transaction =
  < cancel : unit Lwt.t

  ; error_code : (int32 * string) React.event Lwt.t

  ; finished : (int32 * int32) React.event Lwt.t

  ; get_updates : int64 -> unit Lwt.t

  ; package : (int32 * string * string) React.event Lwt.t

  ; percentage : int32 React.signal Lwt.t

  ; status : int32 React.signal Lwt.t

  ; update_packages : string list -> unit Lwt.t
    
  >

type t =
  < can_authorize : string -> int32 Lwt.t

  ; create_transaction : transaction Lwt.t
                         
  ; get_transaction_list : string list list Lwt.t

  ; repo_list_changed : unit React.event Lwt.t

  ; restart_schedule : unit React.event Lwt.t

  ; transaction_list_changed : string list React.event Lwt.t

  ; updates_changed : unit React.event Lwt.t
  
  >

val create : unit -> (t, [> `PackageKit of string ]) Lwt_result.t
