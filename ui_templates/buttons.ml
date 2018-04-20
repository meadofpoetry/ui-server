open Containers
open Components

let base_class = "mdc-apply-button"

(** Creates apply button which is disabled when signal value is None.
    Sends provided request when clicked **)
let create_apply : type a b. (a option React.signal) -> (a -> (b,_) Lwt_result.t) -> Button.t =
  fun s f ->
  let b  = new Button.t ~label:"Применить" () in
  let () = b#add_class base_class in
  let _  = React.S.map (function Some _ -> b#set_disabled false
                               | None   -> b#set_disabled true) s
  in
  let _  = React.E.map (fun _ -> Option.iter (fun s -> f s |> ignore) @@ React.S.value s) b#e_click in
  b
