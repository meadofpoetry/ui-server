open Js_of_ocaml
open Js_of_ocaml_lwt
open Containers
open Components
open Tabs
open Tyxml_js

module Markup = Components_markup.Scaffold.Make(Xml)(Svg)(Html)

module App_bar = struct

  let attach () : Top_app_bar.t =
    Dom_html.getElementById "main-top-app-bar"
    |> Top_app_bar.attach ~tolerance:{ up = 5; down = 5 }

end

type drawer_elevation =
  | Full_height
  | Clipped

type drawer_breakpoint =
  int * drawer_type
and drawer_type =
  | Modal
  | Dismissible
  | Permanent

let drawer_type_to_enum = function
  | Modal -> 0
  | Dismissible -> 1
  | Permanent -> 2

let equal_drawer_type (a : drawer_type as 'a) (b : 'a) : bool =
  (drawer_type_to_enum a) = (drawer_type_to_enum b)

let mobile_breakpoint = 1160

let default_breakpoints : drawer_breakpoint list =
  [(mobile_breakpoint, Modal)]

module Selector = struct
  open Markup.CSS

  let not_found (name : string) : string =
    Printf.sprintf "%s: %s not found" root name

  let by_class (elt : Dom_html.element Js.t) (c : string)
      : Dom_html.element Js.t =
    elt##querySelector (Js.string ("." ^ c))
    |> (fun x -> Js.Opt.get x (fun () -> failwith (not_found c)))

end

class t ?(drawer : #Drawer.t option)
        ?(drawer_elevation : drawer_elevation option)
        ?(drawer_breakpoints = Modal, [])
        ?(top_app_bar : #Top_app_bar.t option)
         (* ?(body : #Widget.t option) *)
        (elt : #Dom_html.element Js.t)
        () =
  let drawer, setup_drawer_elevation =
    match drawer with
    | Some d ->
       Some d, (match drawer_elevation with
                | None -> Some Clipped
                | Some x -> Some x)
    | None ->
       let drawer =
         Js.string ("." ^ Drawer.Markup.CSS.root)
         |> (fun s -> elt##querySelector s)
         |> Js.Opt.to_option
         |> Option.map Drawer.attach in
       drawer, None in
  let drawer_frame_full_height =
    Selector.by_class elt Markup.CSS.drawer_frame_full_height in
  let drawer_frame_clipped =
    Selector.by_class elt Markup.CSS.drawer_frame_clipped in
  let app_content_inner =
    Selector.by_class elt Markup.CSS.app_content_inner in
  object(self)

    (* Nodes *)
    val mutable modal_drawer_wrapper = None

    (* Event listeners *)
    val mutable menu_click_listener = None
    val mutable resize_listener = None

    val mutable drawer_type = fst drawer_breakpoints

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      begin match setup_drawer_elevation with
      | None -> ()
      | Some e -> Option.iter (self#setup_drawer e) drawer
      end;
      Option.iter self#setup_app_bar top_app_bar;
      ignore drawer_breakpoints;

    method! destroy () : unit =
      super#destroy ()

    (** Returns current drawer elevation *)
    method drawer_elevation : drawer_elevation option =
      Option.map (fun (d : #Drawer.t) ->
          match d#parent_element with
          | None -> assert false
          | Some p ->
             let has_class s = Js.(to_bool @@ p##.classList##contains (string s)) in
             Markup.CSS.(
               if has_class drawer_frame_full_height
               then Full_height
               else if has_class drawer_frame_clipped
               then Clipped
               else failwith "mdc-scaffold: bad drawer parent"))
        drawer

    method set_drawer_elevation (e : drawer_elevation) : unit =
      Option.iter (self#setup_drawer e) drawer

    (* Private methods *)

    method private setup_app_bar (app_bar : #Top_app_bar.t) : unit =
      let leading = match app_bar#leading, drawer with
        | None, Some _ ->
           let icon = Icon.SVG.(create_simple Path.menu) in
           let w = new Icon_button.t ~icon () in
           w#add_class Top_app_bar.Markup.navigation_icon_class;
           Some w
        | _ -> None in
      Option.iter app_bar#set_leading leading;
      begin match app_bar#leading, drawer with
      | Some l, Some d ->
         let listener = l#listen_click_lwt (fun _ _ -> d#toggle_await ()) in
         menu_click_listener <- Some listener;
      | _ -> ()
      end;
      let insert = Widget.Element.insert_child_at_index in
      insert app_content_inner 0 app_bar#root

    method private place_drawer
                     (drawer : #Side_sheet.Parent.t)
                     (elevation : drawer_elevation)
                     (typ : drawer_type) : unit =
      let parent = match elevation with
        | Clipped -> drawer_frame_clipped
        | Full_height -> drawer_frame_full_height in
      match typ with
      | Permanent | Dismissible ->
         Option.iter (fun div ->
             Js.Opt.iter div##.parentNode (fun x -> Dom.removeChild x div);
             modal_drawer_wrapper <- None)
           modal_drawer_wrapper;
         Dom.insertBefore parent drawer#root parent##.firstChild;
      | Modal ->
         let div =
           Html.(div [ Of_dom.of_element drawer#root
                     ; Drawer.Markup.create_scrim () ])
           |> To_dom.of_element in
         modal_drawer_wrapper <- Some div;
         Dom.insertBefore parent div parent##.firstChild

    method private set_drawer_type_
                     (elevation : drawer_elevation)
                     (drawer : #Side_sheet.Parent.t) = function
      | Permanent ->
         Option.iter (fun x -> x#hide_leading ()) top_app_bar;
         drawer#set_permanent ();
         self#place_drawer drawer elevation Permanent;
      | Dismissible ->
         Option.iter (fun x -> x#show_leading ()) top_app_bar;
         drawer#set_dismissible ();
         self#place_drawer drawer elevation Dismissible;
      | Modal ->
         self#place_drawer drawer elevation Modal;
         Option.iter (fun x -> x#show_leading ()) top_app_bar;
         drawer#set_modal ()

    method private setup_drawer (elevation : drawer_elevation)
                     (drawer : #Side_sheet.Parent.t) : unit =
      self#set_drawer_type_ elevation drawer drawer_type

  end

(** Create new scaffold widget from scratch *)
let make ?drawer ?drawer_elevation ?drawer_breakpoints
      ?(top_app_bar : #Top_app_bar.t option)
      (* ?(body : #Widget.t option) *)
      () =
  let elt =
    Markup.(
      create
        [create_drawer_frame ~full_height:true
           [create_app_content ~outer:true
              [create_drawer_frame ~clipped:true
                 [create_app_content ~inner:true [] ()] ()]
              ()]
           ()]
        ())
    |> To_dom.of_element in
  new t ?drawer ?drawer_elevation ?drawer_breakpoints
    ?top_app_bar elt ()

(** Attach scaffold widget to existing element *)
let attach (elt : #Dom_html.element Js.t) : t =
  new t elt ()
