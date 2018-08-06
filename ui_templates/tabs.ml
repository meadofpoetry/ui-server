open Containers
open Components

module Markup = Components_markup

let create_simple_tabs (tabs:('a,Widget.t) Tab.t list) =
  let hide   = fun w -> w#style##.display := Js.string "none" in
  (* let show   = fun w -> w#style##.display := Js.string "" in *)
  let _class = "mdc-simple-tabs" in
  let bar  = new Tab_bar.t ~tabs () in
  let ()   = List.iter (fun t -> hide t#value) bar#scroller#tabs in
  let body = Dom_html.createDiv Dom_html.document |> Widget.create in
  (* let _    = React.S.diff (fun n o -> Option.iter (fun o -> hide o#value) o;
   *                                     Option.iter (fun n -> show n#value) n)
   *                         bar#s_active
   * in *)
  (* let ()   = Option.iter (fun t -> show t#value) @@ React.S.value bar#s_active in *)
  let ()   = List.iter (fun t -> body#append_child t#value) bar#scroller#tabs in
  let ()   = body#add_class @@ Markup.CSS.add_element _class "body" in
  let ()   = bar#add_class  @@ Markup.CSS.add_element _class "tabs" in
  object(self)
    inherit Widget.t (Dom_html.createDiv Dom_html.document) () as super
    method! layout () = super#layout (); (* scrl#layout (); *) body#layout ()
    initializer
      self#append_child bar;
      self#append_child body;
      self#add_class _class
  end
