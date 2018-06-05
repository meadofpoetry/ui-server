open Containers
open Board_types
open Api_js.Requests.Json
open Requests_common
open Common

open Api_utils.Jitter

let req_to_uri control req = req_to_uri control (`Jitter req)
