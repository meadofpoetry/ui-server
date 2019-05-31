type msg_type =
  | HELLO
  | WELCOME
  | ABORT
  | GOODBYE
  | ERROR
  | SUBSCRIBE
  | SUBSCRIBED
  | UNSUBSCRIBE
  | UNSUBSCRIBED
  | EVENT

let msg_type_to_enum = function
  | HELLO -> 1
  | WELCOME -> 2
  | ABORT -> 3
  | GOODBYE -> 6
  | ERROR -> 8
  | SUBSCRIBE -> 32
  | SUBSCRIBED -> 33
  | UNSUBSCRIBE -> 34
  | UNSUBSCRIBED -> 35
  | EVENT -> 36

let msg_type_of_enum = function
  | 1 -> Some HELLO
  | 2 -> Some WELCOME
  | 3 -> Some ABORT
  | 6 -> Some GOODBYE
  | 8 -> Some ERROR
  | 32 -> Some SUBSCRIBE
  | 33 -> Some SUBSCRIBED
  | 34 -> Some UNSUBSCRIBE
  | 35 -> Some UNSUBSCRIBED
  | 36 -> Some EVENT
  | _ -> None

type role =
  | Subscriber
  | Publisher

let role_to_string = function
  | Subscriber -> "subscriber"
  | Publisher -> "publisher"

let invalid_uri = Uri.of_string "wamp.error.invalid_uri"

let no_such_subscription = Uri.of_string "wamp.error.no_such_subscription"

let system_shutdown = Uri.of_string "wamp.close.system_shutdown"

let close_realm = Uri.of_string "wamp.close.close_realm"

let goodbye_and_out = Uri.of_string "wamp.close.goodbye_and_out"

let protocol_violation = Uri.of_string "wamp.error.protocol_violation"

module Element = struct
  type arr = t list
  and dict = (string * t) list

  and t =
    | Int of int
    | String of string
    | Bool of bool
    | Dict of dict
    | List of arr
end

module type BODY = sig
  type t

  val to_msg : t -> Element.t
  val of_msg : Element.t -> t
end

open Element

type t =
  | Hello of
      { realm : Uri.t
      ; details : dict }
  | Welcome of
      { id : int
      ; details : dict }
  | Abort of
      { details : dict
      ; reason : Uri.t }
  | Goodbye of
      { details : dict
      ; reason : Uri.t }
  | Error of
      { reqtype : int
      ; reqid : int
      ; details : dict
      ; error : Uri.t
      ; args : arr
      ; kw_args : dict }
  | Subscribe of
      { reqid: int
      ; options: dict
      ; topic: Uri.t }
  | Subscribed of
      { reqid: int
      ; id: int }
  | Unsubscribe of
      { reqid: int
      ; id: int }
  | Unsubscribed of int
  | Event of
      { subid: int
      ; pubid: int
      ; details: dict
      ; args : arr
      ; kw_args : dict }

let msg_type = function
  | Hello _ -> HELLO
  | Welcome _ -> WELCOME
  | Abort _ -> ABORT
  | Goodbye _ -> GOODBYE
  | Error _ -> ERROR
  | Subscribe _ -> SUBSCRIBE
  | Subscribed _ -> SUBSCRIBED
  | Unsubscribe _ -> UNSUBSCRIBE
  | Unsubscribed _ -> UNSUBSCRIBED
  | Event _ -> EVENT

let hello ~realm ~details = Hello { realm; details }

let hello_roles ~realm ~roles =
  let roles =
    Dict (ListLabels.map roles ~f:(fun r -> role_to_string r, Dict [])) in
  hello ~realm ~details:["roles", roles]

let welcome ~id ~details = Welcome { id; details }

let abort ~details ~reason = Abort { details; reason }

let goodbye ~details ~reason = Goodbye { details; reason }

let error ?(details = []) ?(args = []) ?(kw_args = []) ~reqtype ~reqid ~error () =
  Error { reqtype ; reqid ; details ; error ; args ; kw_args }

let subscribe ?(reqid = Random.bits ()) ?(options = []) topic =
  reqid, Subscribe { reqid; options; topic }

let subscribed ~reqid ~id = Subscribed { reqid ; id }

let unsubscribe ~reqid ~id = Unsubscribe { reqid ; id }

let unsubscribed ~reqid = Unsubscribed reqid

let event ~subid ~pubid ~details ~args ~kw_args =
  Event { subid ; pubid ; details ; args ; kw_args }

let remaining_args = function
  | [List args] -> args, []
  | [List args; Dict kw_args] -> args, kw_args
  | _ -> [], []

module Make (B : BODY) = struct
  open Element

  let parse = function
    | List (Int typ :: content) ->
      begin match msg_type_of_enum typ with
        | None ->
          (Error Printf.(sprintf "msg_of_yojson: invalid msg type %d" typ) : _ result)
        | Some HELLO -> begin match content with
            | [String uri; Dict details] ->
              let realm = Uri.of_string uri in
              Ok (hello ~realm ~details)
            | _ -> Error "parse: HELLO"
          end
        | Some WELCOME -> begin match content with
            | [ Int id; Dict details] ->
              Ok (welcome ~id ~details)
            | _ -> Error "parse: WELCOME"
          end
        | Some ABORT -> begin match content with
            | [ Dict details; String reason] ->
              let reason = Uri.of_string reason in
              Ok (abort ~details ~reason)
            | _ -> Error "parse: ABORT"
          end
        | Some GOODBYE -> begin
            match content with
            | [ Dict details; String reason] ->
              let reason = Uri.of_string reason in
              Ok (goodbye ~details ~reason)
            | _ -> Error "parse: GOODBYE"
          end
        | Some ERROR -> begin
            match content with
            | Int reqtype :: Int reqid :: Dict details :: String uri :: tl ->
              let uri = Uri.of_string uri in
              let args, kw_args = remaining_args tl in
              Ok (error ~reqtype ~reqid ~details ~error:uri ~args ~kw_args ())
            | _ -> Error "parse: ERROR"
          end
        | Some SUBSCRIBE -> begin
            match content with
            | [ Int reqid; Dict options; String topic] ->
              let topic = Uri.of_string topic in
              Ok (snd @@ subscribe ~reqid ~options topic)
            | _ -> Error "parse: PUBLISH"
          end
        | Some SUBSCRIBED -> begin
            match content with
            | [ Int reqid; Int id] ->
              Ok (subscribed ~reqid ~id)
            | _ -> Error "parse: SUBSCRIBED"
          end
        | Some UNSUBSCRIBE -> begin
            match content with
            | [ Int reqid; Int id] ->
              Ok (unsubscribe ~reqid ~id)
            | _ -> Error "parse: UNSUBSCRIBE"
          end
        | Some UNSUBSCRIBED -> begin
            match content with
            | [ Int reqid] -> Ok (unsubscribed reqid)
            | _ -> Error "parse: UNSUBSCRIBED"
          end
        | Some EVENT -> begin
            match content with
            |  Int subid :: Int pubid :: Dict details :: tl ->
              let args, kw_args = remaining_args tl in
              Ok (event ~subid ~pubid ~details ~args ~kw_args)
            | _ -> Error "parse: EVENT"
          end
      end
    | msg -> Error "parse: msg must be a List"

  let to_element = function
    | Hello { realm; details } ->
      List [ Int (msg_type_to_enum HELLO)
           ; String (Uri.to_string realm)
           ; Dict details]
    | Welcome { id; details } ->
      List [ Int (msg_type_to_enum WELCOME)
           ; Int id
           ; Dict details ]
    | Abort { details; reason } ->
      List [ Int (msg_type_to_enum ABORT)
           ; Dict details
           ; String (Uri.to_string reason) ]
    | Goodbye { details; reason } ->
      List [ Int (msg_type_to_enum GOODBYE)
           ; Dict details
           ; String (Uri.to_string reason) ]
    | Error { reqtype; reqid; details; error; args; kw_args } ->
      List [ Int (msg_type_to_enum ERROR)
           ; Int reqtype
           ; Int reqid
           ; Dict details
           ; String (Uri.to_string error)
           ; List args
           ; Dict kw_args ]
    | Subscribe { reqid; options; topic } ->
      List [ Int (msg_type_to_enum SUBSCRIBE)
           ; Int reqid
           ; Dict options
           ; String (Uri.to_string topic) ]
    | Subscribed { reqid; id } ->
      List [ Int (msg_type_to_enum SUBSCRIBED)
           ; Int reqid
           ; Int id]
    | Unsubscribe { reqid; id } ->
      List [ Int (msg_type_to_enum UNSUBSCRIBE)
           ; Int reqid
           ; Int id ]
    | Unsubscribed reqid ->
      List [ Int (msg_type_to_enum UNSUBSCRIBED)
           ; Int reqid ]
    | Event { subid; pubid; details; args; kw_args } ->
      List [ Int (msg_type_to_enum EVENT)
           ; Int subid
           ; Int pubid
           ; Dict details
           ; List args
           ; Dict kw_args ]

  let serialize t = B.of_msg (to_element t)

end
