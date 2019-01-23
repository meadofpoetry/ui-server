open Js_of_ocaml
open Tyxml_js
open Containers

module Markup = Components_markup.Scaffold.Make(Xml)(Svg)(Html)

type drawer_elevation =
  | Full_height
  | Clipped

let equal_drawer_elevation (a : drawer_elevation as 'a) (b : 'a) =
  match a, b with
  | Full_height, Full_height | Clipped, Clipped -> true
  | _ -> false

type drawer_breakpoint =
  int * Side_sheet.typ

let drawer_type_to_enum : Side_sheet.typ -> int = function
  | Modal -> 0
  | Dismissible -> 1
  | Permanent -> 2

let equal_drawer_type (a : Side_sheet.typ as 'a) (b : 'a) : bool =
  (drawer_type_to_enum a) = (drawer_type_to_enum b)

let mobile_breakpoint = 1160

let default_breakpoints : drawer_breakpoint list =
  [(mobile_breakpoint, Modal)]

module Selector = struct
  open Markup.CSS

  let not_found (name : string) : string =
    Printf.sprintf "%s: %s not found" root name

  let by_class_opt (elt : Dom_html.element Js.t) (c : string)
      : Dom_html.element Js.t option =
    elt##querySelector (Js.string ("." ^ c))
    |> Js.Opt.to_option

  let by_class (elt : Dom_html.element Js.t) (c : string)
      : Dom_html.element Js.t =
    match by_class_opt elt c with
    | Some x -> x
    | None -> failwith (not_found c)

end

let attach_top_app_bar (elt : Dom_html.element Js.t) () =
  Selector.by_class_opt elt Top_app_bar.Markup.CSS.root
  |> Option.map Top_app_bar.attach

let attach_drawer (elt : Dom_html.element Js.t) () =
  Selector.by_class_opt elt Drawer.Markup.CSS.root
  |> Option.map Drawer.attach

let attach_side_sheet (elt : Dom_html.element Js.t) () =
  Selector.by_class_opt elt Side_sheet.Markup.CSS.root
  |> Option.map Side_sheet.attach

let attach_body (app_content_inner : Dom_html.element Js.t) () =
  match Js.Opt.to_option app_content_inner##.firstChild with
  | None -> None
  | Some node ->
     match node##.nodeType with
     | ELEMENT -> Some (Widget.create (Js.Unsafe.coerce node))
     | _ -> None

class t ?(drawer : #Drawer.t option)
        ?(drawer_elevation : drawer_elevation option)
        ?(drawer_breakpoints = Side_sheet.Modal, [])
        ?(side_sheet : #Side_sheet.t option)
        ?(side_sheet_elevation : drawer_elevation option)
        ?(side_sheet_breakpoints = Side_sheet.Modal, [])
        ?(top_app_bar : #Top_app_bar.t option)
        ?(body : #Widget.t option)
        (elt : #Dom_html.element Js.t)
        () =
  let drawer_frame_full_height =
    Selector.by_class elt Markup.CSS.drawer_frame_full_height in
  let drawer_frame_clipped =
    Selector.by_class elt Markup.CSS.drawer_frame_clipped in
  let app_content_outer =
    Selector.by_class elt Markup.CSS.app_content_outer in
  let app_content_inner =
    Selector.by_class elt Markup.CSS.app_content_inner in
  object(self)

    (* Nodes *)

    val mutable modal_drawer_wrapper = None

    val mutable top_app_bar =
      Option.or_lazy
        ~else_:(attach_top_app_bar elt)
        top_app_bar
    val mutable drawer =
      Option.or_lazy
        ~else_:(attach_drawer elt)
        drawer
    val mutable side_sheet =
      Option.or_lazy
        ~else_:(attach_side_sheet elt)
        side_sheet
    val mutable body =
      Option.or_lazy
        ~else_:(attach_body app_content_inner)
        body

    (* Event listeners *)
    val mutable menu_click_listener = None
    val mutable resize_listener = None

    val mutable drawer_type = fst drawer_breakpoints
    val mutable side_sheet_type = fst side_sheet_breakpoints

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      (* Setup top app bar *)
      Option.iter self#setup_app_bar top_app_bar;
      (* Setup drawer *)
      begin match drawer,
                  drawer_elevation,
                  self#drawer_elevation with
      | None, _, _ | _, None, Some _ -> ()
      | Some w, init, None ->
         let typ = drawer_type in
         let init = Option.get_or ~default:Clipped init in
         self#set_drawer_type_ ~is_leading:true init w typ
      | Some w, Some init, Some cur ->
         let typ = drawer_type in
         if not (equal_drawer_elevation init cur)
         then self#set_drawer_type_ ~is_leading:true init w typ
      end;
      (* Setup side sheet *)
      begin match side_sheet,
                  side_sheet_elevation,
                  self#side_sheet_elevation with
      | None, _, _ | _, None, Some _ -> ()
      | Some w, init, None ->
         let typ = side_sheet_type in
         let init = Option.get_or ~default:Clipped init in
         self#set_drawer_type_ ~is_leading:false init w typ
      | Some w, Some init, Some cur ->
         let typ = side_sheet_type in
         if not (equal_drawer_elevation init cur)
         then self#set_drawer_type_ ~is_leading:false init w typ
      end;
      (* Setup body *)
      Option.iter self#set_body body

    method! destroy () : unit =
      super#destroy ()

    (* Widgets *)

    method top_app_bar : Top_app_bar.t option =
      top_app_bar

    method drawer : Drawer.t option =
      drawer

    method side_sheet : Side_sheet.t option =
      side_sheet

    method set_side_sheet : 'a. ?typ:Side_sheet.typ ->
                            ?elevation:drawer_elevation ->
                            (#Side_sheet.Parent.t as 'a) ->
                            unit =
      fun ?typ ?elevation (side_sheet : #Side_sheet.Parent.t) ->
      let side_sheet = (side_sheet :> Side_sheet.Parent.t) in
      let typ = Option.get_or ~default:side_sheet_type typ in
      let elevation = Option.get_or ~default:Clipped elevation in
      self#set_drawer_type_ ~is_leading:false elevation side_sheet typ

    method body : Widget.t option =
      body

    method set_body : 'a. (#Widget.t as 'a) -> unit =
      fun (body : #Widget.t) ->
      Widget.Element.remove_children app_content_inner;
      Dom.appendChild app_content_inner body#root

    method drawer_elevation : drawer_elevation option =
      self#drawer_elevation_ self#drawer

    method side_sheet_elevation : drawer_elevation option =
      self#drawer_elevation_ self#side_sheet

    (* Private methods *)

    method drawer_elevation_ (drawer : #Side_sheet.Parent.t option)
           : drawer_elevation option =
      Option.flat_map (fun (d : #Side_sheet.Parent.t) ->
          match d#parent_element with
          | None -> None
          | Some p ->
             let has_class s = Js.(to_bool @@ p##.classList##contains (string s)) in
             Markup.CSS.(
               if has_class drawer_frame_full_height
               then Some Full_height
               else if has_class drawer_frame_clipped
               then Some Clipped
               else None))
        drawer

    method private setup_app_bar (app_bar : #Top_app_bar.t) : unit =
      let leading = match app_bar#leading, drawer with
        | None, Some _ ->
           let icon = Icon.SVG.(create_simple Path.menu) in
           let w = new Icon_button.t ~icon () in
           w#add_class Top_app_bar.Markup.CSS.navigation_icon;
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
      insert app_content_outer 0 app_bar#root

    method private place_drawer
                     ?(is_leading = false)
                     (drawer : #Side_sheet.Parent.t)
                     (elevation : drawer_elevation)
                     (typ : Side_sheet.typ) : unit =
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
         let scrim =
           if is_leading
           then Drawer.Markup.create_scrim ()
           else Side_sheet.Markup.create_scrim () in
         let div =
           Html.(div [Of_dom.of_element drawer#root; scrim])
           |> To_dom.of_element in
         modal_drawer_wrapper <- Some div;
         Dom.insertBefore parent div parent##.firstChild

    method private set_drawer_type_
                     ?(is_leading = false)
                     (elevation : drawer_elevation)
                     (drawer : #Side_sheet.Parent.t)
                     (typ : Side_sheet.typ) : unit =
      match typ with
      | Permanent ->
         drawer#set_permanent ();
         self#place_drawer ~is_leading drawer elevation Permanent;
         if is_leading then
           Option.iter (fun x -> x#hide_leading ()) top_app_bar;
      | Dismissible ->
         drawer#set_dismissible ();
         self#place_drawer ~is_leading drawer elevation Dismissible;
         if is_leading then
           Option.iter (fun x -> x#show_leading ()) top_app_bar;
      | Modal ->
         self#place_drawer ~is_leading drawer elevation Modal;
         drawer#set_modal ();
         if is_leading then
           Option.iter (fun x -> x#show_leading ()) top_app_bar;

    method private handle_resize () : unit Lwt.t =
      Lwt.return_unit

  end

(** Create new scaffold widget from scratch *)
let make ?drawer ?drawer_elevation ?drawer_breakpoints
      ?side_sheet ?side_sheet_elevation ?side_sheet_breakpoints
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
    ?side_sheet ?side_sheet_elevation ?side_sheet_breakpoints
    ?top_app_bar elt ()

(** Attach scaffold widget to existing element *)
let attach (elt : #Dom_html.element Js.t) : t =
  new t elt ()
