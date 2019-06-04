open Js_of_ocaml
open Components

let ( % ) f g x = f (g x)

(* TODO remove after 4.08 *)
module Option = struct
  let equal f a b = match a, b with
    | None, None -> true
    | Some _, None | None, Some _ -> false
    | Some a, Some b -> f a b
  let iter f = function None -> () | Some x -> f x
  let map f = function None -> None | Some x -> Some (f x)
end

type 'a dynamic_value = (unit -> #Widget.t as 'a)

let create_simple ?body (tabs : (#Widget.t * Tab.t) list) =
  let hide = fun w -> w#root##.style##.display := Js.string "none" in
  let show = fun w -> w#root##.style##.display := Js.string "" in
  let scroller = Tab_scroller.make (List.map snd tabs) in
  let signal, push = React.S.create None in
  let bar = Tab_bar.make ~on_change:(fun _ x ->
      push x#active_tab_index;
      Lwt.return_unit)
      scroller in
  let body = match body with
    | Some x -> x
    | None -> Widget.create_div () in
  List.iter (fun (w, _) -> hide w; body#append_child w) tabs;
  let _s =
    React.S.diff (fun n o ->
        Option.iter (fun o ->
            match List.nth_opt tabs o with
            | None -> ()
            | Some (x, _) -> hide x) o;
        Option.iter (fun n ->
            match List.nth_opt tabs n with
            | None -> ()
            | Some (x, _) -> show x) n)
      signal in
  Option.iter (fun i -> match List.nth_opt tabs i with
      | None -> ()
      | Some (w, _) -> show w) bar#active_tab_index;
  bar, body

let create_dynamic ?body (tabs : ((unit -> #Widget.t) * Tab.t) list) =
  let signal, push = React.S.create None in
  let scroller = Tab_scroller.make (List.map snd tabs) in
  let bar = Tab_bar.make ~on_change:(fun _ x ->
      push x#active_tab_index;
      Lwt.return_unit)
      scroller in
  let body = match body with
    | Some x -> x
    | None -> Widget.create_div () in
  let s =
    React.S.map ~eq:(Option.equal Widget.equal) (function
        | None -> None
        | Some i ->
          match List.nth_opt tabs i with
          | None -> None
          | Some (f, _) -> Some (f ()))
      signal in
  React.S.diff (fun n o ->
      Option.iter (fun n -> n#layout (); body#append_child n) n;
      Option.iter (fun o -> o#destroy (); body#remove_child o) o) s
  |> (fun e -> body#set_on_destroy (fun () -> React.E.stop ~strong:true e));
  Option.iter (fun w -> w#layout (); body#append_child w) @@ React.S.value s;
  bar, body

let wrap (bar : Tab_bar.t) body =
  object(self)
    inherit Widget.t Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      self#append_child bar;
      self#append_child body

    method! destroy () : unit =
      super#destroy ();
      bar#destroy ();
      body#destroy ()

  end
