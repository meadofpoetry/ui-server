open Js_of_ocaml
open Js_of_ocaml_lwt
open Containers
open Components
open Tabs

let screen_width_breakpoint = 1160

let main_top_app_bar_class = "main-top-app-bar"

class type container =
  object
    inherit Widget.t
    method content : Widget.t option
    method set_content : Widget.t -> unit
  end

type 'a value = string * (unit -> (#Widget.t as 'a))

type ('a, 'b) tab = ('a, 'b value) Tab.t

type ('a, 'b) page_content =
  [ `Static of (#Widget.t as 'b) list
  | `Dynamic of ('a, 'b) tab list
  ]

let hash_of_tab (tab : ('a, 'b) tab) : string =
  fst @@ tab#value

let widget_of_tab (tab : ('a, 'b) tab) : Widget.t =
  Widget.coerce @@ (snd tab#value) ()

let set_active_page container tab_bar =
  let hash =
    Dom_html.window##.location##.hash
    |> Js.to_string
    |> String.drop 1 in
  let default = List.head_opt tab_bar#tabs in
  let active : ('a, 'b) tab option =
    List.find_opt (fun (tab : ('a, 'b) tab) ->
        String.equal hash @@ hash_of_tab tab) tab_bar#tabs
    |> fun x -> Option.choice [ x; default ] in
  begin match active with
  | None -> ()
  | Some tab ->
     container#set_content @@ widget_of_tab tab;
     if not tab#active
     then tab_bar#set_active_tab tab |> ignore
  end

let set_hash container tab_bar hash =
  let history = Dom_html.window##.history in
  let hash = "#" ^ hash in
  history##replaceState Js.undefined (Js.string "") (Js.some @@ Js.string hash);
  set_active_page container tab_bar

let switch_tab container tab_bar =
  React.E.map Fun.(set_hash container tab_bar % hash_of_tab)
  @@ React.E.fmap Fun.id
  @@ React.S.changes tab_bar#s_active_tab

let create_tab_row (container : container) (tabs : ('a, 'b) tab list) =
  let open Tabs in
  let bar = new Tab_bar.t ~align:Start ~tabs () in
  set_active_page container bar;
  let section = new Top_app_bar.Section.t ~align:`Start ~widgets:[bar] () in
  let row = new Top_app_bar.Row.t ~sections:[section] () in
  row, switch_tab container bar

let get_arbitrary () : container =
  try
    let elt = Dom_html.getElementById "arbitrary-content" in
    object(self)
      val mutable _content = None
      inherit Widget.t elt ()

      method content : Widget.t option = _content
      method set_content (w : Widget.t) =
        begin match _content with
        | Some c -> self#remove_child c; c#destroy ()
        | None -> ()
        end;
        _content <- Some w;
        self#append_child w;
        w#layout ()
    end
  with e -> print_endline "no arbitrary"; raise e

let get_navigation_drawer () : Drawer.t =
  try Drawer.attach @@ Dom_html.getElementById "main-drawer"
  with e -> print_endline "no sidebar"; raise e

let get_toolbar () : Top_app_bar.Standard.t =
  try
    Dom_html.getElementById "main-top-app-bar"
    |> Top_app_bar.Standard.attach ~tolerance:{ up = 5; down = 5 }
  with e -> print_endline "no toolbar"; raise e

type drawer_type = Modal | Dismissible

let equal_drawer_type (a : drawer_type as 'a) (b : 'a) : bool =
  match a, b with
  | Modal, Modal | Dismissible, Dismissible -> true
  | _ -> false

class t (content : ('a, 'b) page_content) () =
  let s_nav_drawer_class, set_nav_drawer_class =
    (if Dom_html.document##.body##.offsetWidth > screen_width_breakpoint
     then Dismissible else Modal)
    |> React.S.create in
  let main =
    try Dom_html.getElementById "main-content"
    with e -> print_endline "no main"; raise e in
  let arbitrary = get_arbitrary () in
  let toolbar = get_toolbar () in
  let navigation_drawer = get_navigation_drawer () in
  let title = toolbar#get_child_element_by_id "page-title"
              |> Option.get_exn
              |> Widget.create in
  object(self)

    val mutable scrim = None

    (* Event listeners *)
    val mutable menu_click_listener = None
    val mutable resize_listener = None

    (* Signals *)
    val mutable s_state = None

    inherit Widget.t main () as super

    method! init () : unit =
      super#init ();
      (* Init drawer navigation menu *)
      self#init_drawer_navigation_menu ();
      (* Init toolbar menu button *)
      let menu = Dom_html.getElementById "main-menu" in
      Dom_events.listen menu Dom_events.Typ.click (fun _ _ ->
          navigation_drawer#toggle (); true)
      |> (fun x -> menu_click_listener <- Some x);
      (* Set page content *)
      self#set ();
      (* Handle window resize *)
      Lwt_js_events.limited_onresizes ~elapsed_time:0.05
        (fun _ _ -> self#handle_resize ())
      |> (fun x -> resize_listener <- Some x);
      (* Handle modal/dismissible drawer state change *)
      React.S.map self#render_drawer s_nav_drawer_class
      |> (fun x -> s_state <- Some x)

    method! destroy () : unit =
      super#destroy ();
      Option.iter Dom_events.stop_listen menu_click_listener;
      menu_click_listener <- None;
      Option.iter Lwt.cancel resize_listener;
      resize_listener <- None;
      Option.iter (React.S.stop ~strong:true) s_state;
      s_state <- None;
      React.S.stop ~strong:true s_nav_drawer_class

    method title : string =
      Js.to_string Dom_html.document##.title

    method set_title (x : string) : unit =
      Dom_html.document##.title := Js.string x;
      title#set_text_content x

    method arbitrary = arbitrary

    (* Private methods *)

    method private render_drawer (typ : drawer_type) : unit =
      let parent = Dom_html.getElementById "main-panel" in
      match typ with
      | Modal ->
         let div =
           Tyxml_js.Html.(
             div [ Tyxml_js.Of_dom.of_element navigation_drawer#root
                 ; Drawer.Markup.create_scrim () ]) in
         Dom.insertBefore parent
           (Tyxml_js.To_dom.of_element div)
           parent##.firstChild;
         navigation_drawer#set_modal ();
      | Dismissible ->
         (* Remove first child (div with drawer) *)
         let first_child = parent##.firstChild in
         Js.Opt.iter first_child (Dom.removeChild parent);
         (* Insert drawer before content *)
         Dom.insertBefore parent navigation_drawer#root parent##.firstChild;
         navigation_drawer#set_dismissible ();

    method private handle_resize () : unit Lwt.t =
      let value = React.S.value s_nav_drawer_class in
      let screen_width = Dom_html.document##.body##.offsetWidth in
      match value, screen_width with
      | Dismissible, x when x <= screen_width_breakpoint ->
         Lwt.Infix.(
          navigation_drawer#hide_await ()
          >|= (fun () -> set_nav_drawer_class Modal))
      | Modal, x when x > screen_width_breakpoint ->
         Lwt.Infix.(
          navigation_drawer#hide_await ()
          >|= (fun () -> set_nav_drawer_class Dismissible))
      | _ -> Lwt.return_unit

    (** Initialize navigation links *)
    method private init_drawer_navigation_menu () : unit =
      let href = Js.to_string @@ Dom_html.window##.location##.pathname in
      let list_class = Item_list.Markup.base_class in
      let list =
        Option.get_exn
        @@ navigation_drawer#get_child_element_by_class list_class in
      let items = Dom.list_of_nodeList @@ list##.childNodes in
      (* Currently active item *)
      let (item : Dom_html.element Js.t option) =
        List.find_map (fun (item : Dom.node Js.t) ->
            match item##.nodeType with
            | ELEMENT ->
               let (item : Dom_html.element Js.t) = Js.Unsafe.coerce item in
               let href' =
                 Option.map Js.to_string
                 @@ Js.Opt.to_option
                 @@ item##getAttribute (Js.string "href") in
               begin match href' with
               | None -> None
               | Some href' ->
                  if String.equal href' href
                  then Some item else None
               end
            | _ -> None) items in
      (* Style currently active item *)
      Option.iter (fun i ->
          let class' = Js.string Item_list.Markup.Item.activated_class in
          i##.classList##add class')
        item

    method private set () =
      arbitrary#set_empty ();
      match content with
      | `Static widgets ->
         List.iter arbitrary#append_child widgets
      | `Dynamic tabs ->
         let dynamic_class =
           Components_markup.CSS.add_modifier
             main_top_app_bar_class
             "dynamic" in
         toolbar#add_class dynamic_class;
         let row, e = create_tab_row arbitrary tabs in
         self#_keep_e e;
         toolbar#append_child row;
         toolbar#layout ()
  end
