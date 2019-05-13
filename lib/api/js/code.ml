module Informational = struct

  type t =
    [ `Continue
    | `Switching_protocols
    | `Processing
    | `Checkpoint
    | `Request_uri_too_long
    ]

  let to_int : t -> int = function
    | `Continue -> 100
    | `Switching_protocols -> 101
    | `Processing -> 102
    | `Checkpoint -> 103
    | `Request_uri_too_long -> 122

  let of_int : int -> t option = function
    | 100 -> Some `Continue
    | 101 -> Some `Switching_protocols
    | 102 -> Some `Processing
    | 103 -> Some `Checkpoint
    | 122 -> Some `Request_uri_too_long
    | _ -> None

  let to_string : t -> string = function
    | `Continue -> "Continue"
    | `Switching_protocols -> "Switching Protocols"
    | `Processing -> "Processing (WebDAV) (RFC 2518)"
    | `Checkpoint -> "Checkpoint"
    | `Request_uri_too_long -> "Request-URI too long"

end

module Success = struct

  type t =
    [ `OK
    | `Created
    | `Accepted
    | `Non_authoritative_information
    | `No_content
    | `Reset_content
    | `Partial_content
    | `Multi_status
    | `Already_reported
    | `Im_used
    ]

  let to_int : t -> int = function
    | `OK -> 200
    | `Created -> 201
    | `Accepted -> 202
    | `Non_authoritative_information -> 203
    | `No_content -> 204
    | `Reset_content -> 205
    | `Partial_content -> 206
    | `Multi_status -> 207
    | `Already_reported -> 208
    | `Im_used -> 226

  let of_int : int -> t option = function
    | 200 -> Some `OK
    | 201 -> Some `Created
    | 202 -> Some `Accepted
    | 203 -> Some `Non_authoritative_information
    | 204 -> Some `No_content
    | 205 -> Some `Reset_content
    | 206 -> Some `Partial_content
    | 207 -> Some `Multi_status
    | 208 -> Some `Already_reported
    | 226 -> Some `Im_used
    | _ -> None

  let to_string : t -> string = function
    | `OK -> "OK"
    | `Created -> "Created"
    | `Accepted -> "Accepted"
    | `Non_authoritative_information -> "Non-Authoritative Information"
    | `No_content -> "No Content"
    | `Reset_content -> "Reset Content"
    | `Partial_content -> "Partial Content"
    | `Multi_status -> "Multi-Status (WebDAV) (RFC 4918)"
    | `Already_reported -> "Already Reported (WebDAV) (RFC 5842)"
    | `Im_used -> "IM Used (RFC 3229)"
end

module Redirection = struct

  type t =
    [ `Multiple_choices
    | `Moved_permanently
    | `Found
    | `See_other
    | `Not_modified
    | `Use_proxy
    | `Switch_proxy
    | `Temporary_redirect
    | `Resume_incomplete
    ]

  let to_int : t -> int = function
    | `Multiple_choices -> 300
    | `Moved_permanently -> 301
    | `Found -> 302
    | `See_other -> 303
    | `Not_modified -> 304
    | `Use_proxy -> 305
    | `Switch_proxy -> 306
    | `Temporary_redirect -> 307
    | `Resume_incomplete -> 308

  let of_int : int -> t option = function
    | 300 -> Some `Multiple_choices
    | 301 -> Some `Moved_permanently
    | 302 -> Some `Found
    | 303 -> Some `See_other
    | 304 -> Some `Not_modified
    | 305 -> Some `Use_proxy
    | 306 -> Some `Switch_proxy
    | 307 -> Some `Temporary_redirect
    | 308 -> Some `Resume_incomplete
    | _ -> None

  let to_string : t -> string = function
    | `Multiple_choices -> "Multiple Choices"
    | `Moved_permanently -> "Moved Permanently"
    | `Found -> "Found"
    | `See_other -> "See Other"
    | `Not_modified -> "Not Modified"
    | `Use_proxy -> "Use Proxy"
    | `Switch_proxy -> "Switch Proxy"
    | `Temporary_redirect -> "Temporary Redirect"
    | `Resume_incomplete -> "Resume Incomplete"

end

module Client_error = struct

  type t =
    [ `Bad_request
    | `Unauthorized
    | `Payment_required
    | `Forbidden
    | `Not_found
    | `Method_not_allowed
    | `Not_acceptable
    | `Proxy_authentication_required
    | `Request_timeout
    | `Conflict
    | `Gone
    | `Length_required
    | `Precondition_failed
    | `Request_entity_too_large
    | `Request_uri_too_long
    | `Unsupported_media_type
    | `Requested_range_not_satisfiable
    | `Expectation_failed
    | `I_m_a_teapot
    | `Enhance_your_calm
    | `Unprocessable_entity
    | `Locked
    | `Failed_dependency
    | `Upgrade_required
    | `Precondition_required
    | `Too_many_requests
    | `Request_header_fields_too_large
    | `No_response
    | `Retry_with
    | `Blocked_by_windows_parental_controls
    | `Wrong_exchange_server
    | `Client_closed_request
    ]

  let to_int : t -> int = function
    | `Bad_request -> 400
    | `Unauthorized -> 401
    | `Payment_required -> 402
    | `Forbidden -> 403
    | `Not_found -> 404
    | `Method_not_allowed -> 405
    | `Not_acceptable -> 406
    | `Proxy_authentication_required -> 407
    | `Request_timeout -> 408
    | `Conflict -> 409
    | `Gone -> 410
    | `Length_required -> 411
    | `Precondition_failed -> 412
    | `Request_entity_too_large -> 413
    | `Request_uri_too_long -> 414
    | `Unsupported_media_type -> 415
    | `Requested_range_not_satisfiable -> 416
    | `Expectation_failed -> 417
    | `I_m_a_teapot -> 418
    | `Enhance_your_calm -> 420
    | `Unprocessable_entity -> 422
    | `Locked -> 423
    | `Failed_dependency -> 424
    | `Upgrade_required -> 426
    | `Precondition_required -> 428
    | `Too_many_requests -> 429
    | `Request_header_fields_too_large -> 431
    | `No_response -> 444
    | `Retry_with -> 449
    | `Blocked_by_windows_parental_controls -> 450
    | `Wrong_exchange_server -> 451
    | `Client_closed_request -> 499

  let of_int : int -> t option = function
    | 400 -> Some `Bad_request
    | 401 -> Some `Unauthorized
    | 402 -> Some `Payment_required
    | 403 -> Some `Forbidden
    | 404 -> Some `Not_found
    | 405 -> Some `Method_not_allowed
    | 406 -> Some `Not_acceptable
    | 407 -> Some `Proxy_authentication_required
    | 408 -> Some `Request_timeout
    | 409 -> Some `Conflict
    | 410 -> Some `Gone
    | 411 -> Some `Length_required
    | 412 -> Some `Precondition_failed
    | 413 -> Some `Request_entity_too_large
    | 414 -> Some `Request_uri_too_long
    | 415 -> Some `Unsupported_media_type
    | 416 -> Some `Requested_range_not_satisfiable
    | 417 -> Some `Expectation_failed
    | 418 -> Some `I_m_a_teapot
    | 420 -> Some `Enhance_your_calm
    | 422 -> Some `Unprocessable_entity
    | 423 -> Some `Locked
    | 424 -> Some `Failed_dependency
    | 426 -> Some `Upgrade_required
    | 428 -> Some `Precondition_required
    | 429 -> Some `Too_many_requests
    | 431 -> Some `Request_header_fields_too_large
    | 444 -> Some `No_response
    | 449 -> Some `Retry_with
    | 450 -> Some `Blocked_by_windows_parental_controls
    | 451 -> Some `Wrong_exchange_server
    | 499 -> Some `Client_closed_request
    | _ -> None

  let to_string : t -> string = function
    | `Bad_request -> "Bad Request"
    | `Unauthorized -> "Unauthorized"
    | `Payment_required -> "Payment Required"
    | `Forbidden -> "Forbidden"
    | `Not_found -> "Not Found"
    | `Method_not_allowed -> "Method Not Allowed"
    | `Not_acceptable -> "Not Acceptable"
    | `Proxy_authentication_required -> "Proxy Authentication Required"
    | `Request_timeout -> "Request Time-out"
    | `Conflict -> "Conflict"
    | `Gone -> "Gone"
    | `Length_required -> "Length Required"
    | `Precondition_failed -> "Precondition Failed"
    | `Request_entity_too_large -> "Request Entity Too Large"
    | `Request_uri_too_long -> "Request-URI Too Long"
    | `Unsupported_media_type -> "Unsupported Media Type"
    | `Requested_range_not_satisfiable -> "Requested range not satisfiable"
    | `Expectation_failed -> "Expectation Failed"
    | `I_m_a_teapot -> "I'm a teapot (RFC 2324)"
    | `Enhance_your_calm -> "Enhance Your Calm"
    | `Unprocessable_entity -> "Unprocessable Entity (WebDAV) (RFC 4918)"
    | `Locked -> "Locked (WebDAV) (RFC 4918)"
    | `Failed_dependency -> "Failed Dependency (WebDAV) (RFC 4918)"
    | `Upgrade_required -> "Upgrade Required (RFC 2817)"
    | `Precondition_required -> "Precondition Required"
    | `Too_many_requests -> "Too Many Requests"
    | `Request_header_fields_too_large -> "Request Header Fields Too Large"
    | `No_response -> "No Response"
    | `Retry_with -> "Retry With"
    | `Blocked_by_windows_parental_controls -> "Blocked by Windows Parental Controls"
    | `Wrong_exchange_server -> "Wrong Exchange server"
    | `Client_closed_request -> "Client Closed Request"

end

module Server_error = struct

  type t =
    [ `Internal_server_error
    | `Not_implemented
    | `Bad_gateway
    | `Service_unavailable
    | `Gateway_timeout
    | `Http_version_not_supported
    | `Variant_also_negotiates
    | `Insufficient_storage
    | `Loop_detected
    | `Bandwidth_limit_exceeded
    | `Not_extended
    | `Network_authentication_required
    | `Network_read_timeout_error
    | `Network_connect_timeout_error
    ]

  let to_int : t -> int = function
    | `Internal_server_error -> 500
    | `Not_implemented -> 501
    | `Bad_gateway -> 502
    | `Service_unavailable -> 503
    | `Gateway_timeout -> 504
    | `Http_version_not_supported -> 505
    | `Variant_also_negotiates -> 506
    | `Insufficient_storage -> 507
    | `Loop_detected -> 508
    | `Bandwidth_limit_exceeded -> 509
    | `Not_extended -> 510
    | `Network_authentication_required -> 511
    | `Network_read_timeout_error -> 598
    | `Network_connect_timeout_error -> 599

  let of_int : int -> t option = function
    | 500 -> Some `Internal_server_error
    | 501 -> Some `Not_implemented
    | 502 -> Some `Bad_gateway
    | 503 -> Some `Service_unavailable
    | 504 -> Some `Gateway_timeout
    | 505 -> Some `Http_version_not_supported
    | 506 -> Some `Variant_also_negotiates
    | 507 -> Some `Insufficient_storage
    | 508 -> Some `Loop_detected
    | 509 -> Some `Bandwidth_limit_exceeded
    | 510 -> Some `Not_extended
    | 511 -> Some `Network_authentication_required
    | 598 -> Some `Network_read_timeout_error
    | 599 -> Some `Network_connect_timeout_error
    | _ -> None

  let to_string : t -> string = function
    | `Internal_server_error -> "Internal Server Error"
    | `Not_implemented -> "Not Implemented"
    | `Bad_gateway -> "Bad Gateway"
    | `Service_unavailable -> "Service Unavailable"
    | `Gateway_timeout -> "Gateway Time-out"
    | `Http_version_not_supported -> "HTTP Version not supported"
    | `Variant_also_negotiates -> "Variant Also Negotiates (RFC 2295)"
    | `Insufficient_storage -> "Insufficient Storage (WebDAV) (RFC 4918)"
    | `Loop_detected -> "Loop Detected (WebDAV) (RFC 5842)"
    | `Bandwidth_limit_exceeded -> "Bandwidth Limit Exceeded"
    | `Not_extended -> "Not Extended (RFC 2774)"
    | `Network_authentication_required -> "Network Authentication Required"
    | `Network_read_timeout_error -> "Network read timeout error"
    | `Network_connect_timeout_error -> "Network connect timeout error"

end

type t =
  [ Informational.t
  | Success.t
  | Redirection.t
  | Client_error.t
  | Server_error.t
  | `Code of int
  ]

let to_int : t -> int = function
  | `Code x -> x
  | (#Client_error.t as x) -> Client_error.to_int x
  | (#Informational.t as x) -> Informational.to_int x
  | (#Success.t as x) -> Success.to_int x
  | (#Redirection.t as x) -> Redirection.to_int x
  | (#Server_error.t as x) -> Server_error.to_int x

let of_int (i : int) =
  (match i with
   | x when x >= 100 && x < 200 -> (Informational.of_int x :> t option)
   | x when x >= 200 && x < 300 -> (Success.of_int x :> t option)
   | x when x >= 300 && x < 400 -> (Redirection.of_int x :> t option)
   | x when x >= 400 && x < 500 -> (Client_error.of_int x :> t option)
   | x when x >= 500 && x < 600 -> (Server_error.of_int x :> t option)
   | _ -> None)
  |> function
  | None -> `Code i
  | Some x -> x

let to_string : t -> string = function
  | `Code x -> Printf.sprintf "Unknown code (%d)" x
  | (#Informational.t as x) -> Informational.to_string x
  | (#Success.t as x) -> Success.to_string x
  | (#Redirection.t as x) -> Redirection.to_string x
  | (#Client_error.t as x) -> Client_error.to_string x
  | (#Server_error.t as x) -> Server_error.to_string x
