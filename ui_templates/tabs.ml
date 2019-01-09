open Js_of_ocaml
open Containers
open Components

module Markup = Components_markup

type 'a dynamic_value = (unit -> #Widget.t as 'a)

let create_simple ?body (tabs : ('a, Widget.t) Tab.t list) =
  let hide = fun w -> w#style##.display := Js.string "none" in
  let show = fun w -> w#style##.display := Js.string "" in
  let bar = new Tab_bar.t ~tabs () in
  List.iter (fun t -> hide t#value) bar#tabs;
  let body = match body with
    | Some x -> x
    | None -> Widget.create_div () in
  (* FIXME save *)
  let _  =
    React.S.diff (fun n o ->
        Option.iter (fun o -> hide o#value) o;
        Option.iter (fun n -> show n#value) n)
      bar#s_active_tab in
  Option.iter (fun t -> show t#value) @@ React.S.value bar#s_active_tab;
  List.iter (fun t -> body#append_child t#value) bar#tabs;
  bar, body

let create_dynamic ?body (tabs : ('a, 'b dynamic_value) Tab.t list) =
  let bar = new Tab_bar.t ~tabs () in
  let body = match body with
    | Some x -> x
    | None -> Widget.create_div () in
  let s =
    React.S.map ~eq:(Option.equal Widget.equal) (fun tab ->
        Option.map (fun tab -> tab#value ()) tab)
      bar#s_active_tab in
  React.S.diff (fun n o ->
      Option.iter (fun n -> n#layout (); body#append_child n) n;
      Option.iter (fun o -> o#destroy (); body#remove_child o) o) s
  |> (fun e -> body#set_on_destroy (fun () -> React.E.stop ~strong:true e));
  Option.iter (fun w -> w#layout (); body#append_child w) @@ React.S.value s;
  bar, body

let wrap (bar : ('a, 'b) Tab_bar.t) body =
  object(self)
    inherit Widget.t Dom_html.(createDiv document) () as super

    method init () : unit =
      super#init ();
      self#append_child bar;
      self#append_child body

    method destroy () : unit =
      super#destroy ();
      bar#destroy ();
      body#destroy ()

  end
