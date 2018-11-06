open Containers
open Dynamic_grid
open Dashboard_item

type available =
  [ `List   of info list
  | `Groups of (string * info list) list
  ]

class type ['a] factory =
  object
    method create : 'a -> Widget.t item
    method destroy : unit -> unit
    method available : available
    method serialize : 'a -> Yojson.Safe.json
    method deserialize : Yojson.Safe.json -> ('a,string) result
  end

type 'a typ = 'a * t

class ['a] grid (factory : 'a #factory) () =
  let get = fun (i : 'a positioned_item) ->
    factory#create i.item
    |> fun x ->
       Dashboard_item.make x
       |> fun x -> Dynamic_grid.Item.to_item
                     ~close_widget:x#remove#widget
                     ~draggable:true
                     ~resizable:true
                     ~widget:x#widget
                     ~value:(i.item, x)
                     ~pos:i.position
                     () in
  let grid =
    to_grid ~vertical_compact:true
      ~row_height:150
      ~items_margin:(10, 10)
      ~draggable:false
      ~resizable:false
      ~selectable:false
      ~cols:4
      () in
  object(self)

    val mutable _enter_target = Js.null
    val mutable _typ = ""

    inherit ['a typ,
             'a typ Dynamic_grid.Item.t,
             'a positioned_item] Dynamic_grid_abstract.t
              ~items:[] ~get ~grid () as super

    (** API **)

    method editable = match super#draggable, super#resizable with
      | Some false, Some false -> false
      | _ -> true

    method set_editable (x : bool) : unit =
      List.iter (fun i -> (snd i#value)#set_editable x) super#items;
      super#set_draggable @@ Some x;
      super#set_resizable @@ Some x

    (** Private methods *)

    method private move_ghost ?aspect ghost = function
      | None -> ghost#set_pos Position.empty
      | Some epos ->
         let open Position in
         let epos = { epos with x = epos.x / React.S.value self#s_col_w;
                                y = epos.y / React.S.value self#s_row_h } in
         let items = List.map (fun x -> x#pos) self#items in
         let pos = get_free_rect ?aspect ~f:(fun x -> x)
                     epos items self#grid.cols
                     (React.S.value self#s_rows) () in
         (match pos with
          | Some x -> ghost#set_pos x
          | None   -> ghost#set_pos empty)

    initializer
      (let ghost = new Dynamic_grid.Item.cell
                     ~typ:`Ghost
                     ~s_grid:self#s_grid
                     ~s_col_w:self#s_col_w
                     ~s_row_h:self#s_row_h
                     ~pos:Dynamic_grid.Position.empty
                     ()
       in
       ghost#style##.zIndex := Js.string "10000";
       self#append_child ghost;
       self#listen_lwt Widget.Event.dragenter (fun e _ ->
           Dom_html.stopPropagation e;
           Dom.preventDefault e;
           _enter_target <- e##.target;
           Lwt.return_unit)
       |> Lwt.ignore_result;
       self#listen_lwt Widget.Event.dragleave (fun e _ ->
           Dom_html.stopPropagation e;
           Dom.preventDefault e;
           let () = if Equal.physical _enter_target e##.target
                    then ghost#set_pos Dynamic_grid.Position.empty in
           Lwt.return_unit)
       |> Lwt.ignore_result;
       self#listen_lwt Widget.Event.dragover (fun e _ ->
           let a = Js.Unsafe.coerce e##.dataTransfer##.types in
           let l = Js.to_array a |> Array.to_list |> List.map Js.to_string in
           let t = List.find_opt (String.equal Dashboard_add_item.drag_type) l in
           Option.iter (fun t ->
               _typ <- t;
               let p = self#get_event_pos (e :> Dom_html.mouseEvent Js.t) in
               (match p with
                | Some _ -> self#move_ghost ghost p;
                            let gp = ghost#pos in
                            if not @@ Dynamic_grid.Position.(equal gp empty)
                            then Dom.preventDefault e;
                | None -> ())) t;
           Lwt.return_unit)
       |> Lwt.ignore_result;
       self#listen_lwt Widget.Event.drop (fun e _ ->
           Dom.preventDefault e;
           let json = e##.dataTransfer##getData (Js.string _typ)
                      |> Js.to_string
                      |> Yojson.Safe.from_string in
           Result.iter (fun info ->
               let open Dynamic_grid.Position in
               let pos       = ghost#pos in
               if not @@ equal pos empty then
                 match factory#deserialize info.serialized with
                 | Ok x ->
                    begin match self#add { item = x; position = pos } with
                    | Ok x -> (snd x#value)#set_editable self#editable
                    | Error _ -> ()
                    end
                 | Error _ -> ()) (info_of_yojson json);
           ghost#set_pos Dynamic_grid.Position.empty;
           Lwt.return_unit)
       |> Lwt.ignore_result)
  end
