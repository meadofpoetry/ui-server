(* open Js_of_ocaml
 * open Tyxml_js
 * 
 * module Markup = Components_tyxml.Pagination.Make(Xml)(Svg)(Html)
 * 
 * let index_attribute = "data-index"
 * 
 * class t ?(start = 0) ?(visible_pages = 3) ~total () =
 *   let prev_icon = Icon.SVG.(make_simple Path.chevron_left) in
 *   let next_icon = Icon.SVG.(make_simple Path.chevron_right) in
 *   let prev = new Button.t ~icon:prev_icon () in
 *   let next = new Button.t ~icon:next_icon () in
 *   let ul = Markup.create_content [] in
 *   let elt =
 *     To_dom.of_element
 *     @@ Markup.create [ Widget.to_markup prev
 *                      ; ul
 *                      ; Widget.to_markup next ] in
 *   let content = Widget.create (To_dom.of_element ul) in
 *   let current, set_current = React.S.create start in
 *   object(self)
 *     val mutable active_elt = None
 *     val mutable visible : int = visible_pages
 *     val mutable total : int = total
 *     val mutable click_listener = None
 * 
 *     inherit Widget.t elt () as super
 * 
 *     method! init () : unit =
 *       super#init ();
 *       List.iter (fun x -> x#add_class Markup.CSS.button) [prev; next];
 *       self#generate_buttons ();
 *       self#set_active start;
 *       let listener =
 *         content#listen_click_lwt (fun e _ ->
 *             begin match Js.Opt.to_option e##.target, Js.Opt.to_option e##.currentTarget with
 *             | Some target, Some current_target ->
 *                if not @@ Equal.physical target current_target then
 *                  let attr =
 *                    Option.flat_map Fun.(int_of_string_opt % Js.to_string)
 *                    @@ Js.Opt.to_option
 *                    @@ target##getAttribute (Js.string index_attribute) in
 *                  Option.iter self#set_active attr
 *             | _ -> ()
 *             end;
 *             Lwt.return_unit) in
 *       click_listener <- Some listener
 * 
 *     method! destroy () : unit =
 *       super#destroy ();
 *       Option.iter Lwt.cancel click_listener;
 *       click_listener <- None
 * 
 *     (\** Current page number signal *\)
 *     method s_active : int React.signal =
 *       current
 * 
 *     (\** Current page number *\)
 *     method active : int =
 *       React.S.value current
 * 
 *     (\** Sets current page number *\)
 *     method set_active (i : int) : unit =
 *       if self#active <> i then
 *         let query = Printf.sprintf "[%s=\"%d\"]" index_attribute i in
 *         let elt = content#root##querySelector (Js.string query) in
 *         match Js.Opt.to_option elt with
 *         | None -> raise Not_found
 *         | Some elt -> self#set_active_element elt; set_current i
 * 
 *     (\** Private methods *\)
 * 
 *     method private set_active_element (elt : Dom_html.element Js.t) : unit =
 *       let prev = active_elt in
 *       let class' = Js.string Markup.CSS.button_active in
 *       elt##.classList##add class';
 *       Option.iter (fun x -> x##.classList##remove class') prev;
 *       active_elt <- Some elt
 * 
 *     method private generate_buttons () : unit =
 *       let indexes = self#get_visible_indexes () in
 *       let widgets =
 *         List.map (fun (i : int) ->
 *             let label = string_of_int (i + 1) in
 *             let button = new Button.t ~label () in
 *             button#add_class Markup.CSS.button;
 *             button#set_attribute index_attribute (string_of_int i);
 *             button) indexes in
 *       content#set_empty ();
 *       List.iter content#append_child widgets
 * 
 *     method private get_visible_indexes () : int list =
 *       let current = self#active in
 *       let offset = visible / 2 in
 *       let current = max 0 (current - offset) in
 *       let first =
 *         if total - current < visible
 *         then max 0 (total - visible)
 *         else current in
 *       let last = min (first + visible) total in
 *       List.range' first last
 * 
 *   end *)
