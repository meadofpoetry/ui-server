(* let (>|=) x f = Js.Optdef.map x f *)

(* let request_to_obj = let open Js.Unsafe in function *)
(*     | (`List | `Start | `Pause | `Stop) -> [||] *)
(*     | `Info id -> [| ("id", inject id) |] *)
(*     | `Create _ -> [| |] (\* TODO *\) *)
(*     | `Destroy x -> [| ("id", inject x.Mp_destroy.id); *)
(*                        ("permanent", Js.Optdef.option x.Mp_destroy.permanent >|= Js.bool |> inject); *)
(*                        ("secret", Js.Optdef.option x.Mp_destroy.secret >|= Js.string |> inject) |] *)
(*     | `Recording _ -> [||] (\* TODO *\) *)
(*     | `Enable (id,_) -> [| ("id", inject id) |] *)
(*     | `Disable (id,_) -> [| ("id", inject id) |] *)
(*     | `Switch id -> [| ("id", inject id) |] *)
(*     | `Watch (id,_) -> [| ("id", inject id) |] *)

(* type plugin_msg *)

(* let make_plugin_msg *)
(*     ?request *)
(*     ?jsep *)
(*     ?media *)
(*     ?success *)
(*     ?error *)
(*     () = *)
(*   let open Js.Unsafe in *)
(*   let request' = [| ("request", CCOpt.(request *)
(*                                        >|= request_to_string *)
(*                                        |> Js.Optdef.option *)
(*                                        |> fun x -> Js.Optdef.map x Js.string *)
(*                                                    |> inject)) |] in *)
(*   let params = CCOpt.(request *)
(*                       >|= request_to_obj *)
(*                       |> (function *)
(*                           | Some p -> p *)
(*                           | None -> [||])) in *)
(*   let jsep' = ("jsep", Js.Optdef.option jsep |> inject) in *)
(*   let message = ("message",  obj (Array.append request' params)) in *)
(*   let media' = ("media", *)
(*                 obj (match media with *)
(*                     | Some (aud,vid) -> [| ("audioSend", Js.bool aud |> inject); *)
(*                                            ("videoSend", Js.bool vid |> inject) |] *)
(*                     | None -> [||])) in *)
(*   let success' = ("success", Js.Optdef.option success >|= Js.wrap_callback |> inject) in *)
(*   let error' = ("error", Js.Optdef.option error >|= Js.wrap_callback |> inject) in *)
(*   obj [| message; jsep'; media'; success'; error' |] *)


type js_obj = (string * Js.Unsafe.any) array

class type plugin =
  object
    method getId        : unit   -> Js.number Js.t Js.meth
    method getPlugin    : unit   -> Js.js_string Js.t Js.meth
    method send         : js_obj -> unit Js.meth
    method createAnswer : js_obj -> unit Js.meth
  end

class type janus = 
  object
    method getServer    : unit   -> Js.js_string Js.t Js.meth
    method isConnected  : unit   -> bool Js.t Js.meth
    method getSessionId : unit   -> Js.number Js.t Js.meth
    method attach       : js_obj -> unit Js.meth
    method destroy      : js_obj -> unit Js.meth
  end


let init data = Js.Unsafe.(fun_call (js_expr "Janus.init") [| obj data |]) |> ignore

let isWebrtcSupported () = Js.Unsafe.(fun_call (js_expr "Janus.isWebrtcSupported")) [||]

let attachMediaStream el_id stream =
  let el = Dom_html.document##getElementById (Js.string el_id) in
  Js.Unsafe.(fun_call (js_expr "Janus.attachMediaStream") [| inject el; inject stream|])
  
let create data = Js.Unsafe.(new_obj global##._Janus [| obj data |])
