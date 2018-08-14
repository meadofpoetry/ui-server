open Containers
open Components

module Markup = Components_markup

let create_simple ?body (tabs:('a,Widget.t) Tab.t list) =
  let hide   = fun w -> w#style##.display := Js.string "none" in
  let show   = fun w -> w#style##.display := Js.string "" in
  let bar  = new Tab_bar.t ~tabs () in
  let ()   = List.iter (fun t -> hide t#value) bar#scroller#tabs in
  let body = match body with
    | Some x -> x
    | None   -> Widget.create_div () in
  (* FIXME save *)
  let _  = React.S.diff (fun n o -> Option.iter (fun o -> hide o#value) o;
                                    Option.iter (fun n -> show n#value) n)
             bar#scroller#s_active_tab in
  let () = Option.iter (fun t -> show t#value)
           @@ React.S.value bar#scroller#s_active_tab in
  let () = List.iter (fun t -> body#append_child t#value) bar#scroller#tabs in
  bar, body

let wrap_simple (bar, body) =
  let div = Widget.create_div () in
  div#append_child bar;
  div#append_child body;
  div
