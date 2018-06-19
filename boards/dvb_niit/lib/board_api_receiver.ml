open Containers
open Common
open Board_types
open Board_protocol
open Board_api_common
open Api.Interaction
open Api.Redirect

(** API
    POST /receiver/{id}/mode

    GET  /receiver/mode
    GET  /receiver/lock
    GET  /receiver/measures
    GET  /receiver/parameters
    GET  /receiver/plp-list
 *)

type events = receiver_events
