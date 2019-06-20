open Js_of_ocaml
open Components

let ( >>= ) = Lwt.bind

let split_string ~prefix pattern =
  let len = String.length prefix in
  if len > String.length pattern
  then None
  else
    let sub = String.sub pattern 0 len in
    if String.uppercase_ascii sub = String.uppercase_ascii prefix
    then Some (String.sub pattern len (String.length pattern - len))
    else None

class virtual t (elt : Dom_html.element Js.t) () = object(self)

  inherit Widget.t elt () as super

  val virtual ghost  : Dom_html.element Js.t
  val mutable _dnd_typ = ""
  val mutable _dragenter_target = Js.null
  val mutable _drag_listeners = []

  method! initial_sync_with_dom () : unit =
    _drag_listeners <- Events.(
        [ dragenters super#root self#handle_dragenter
        ; dragovers super#root self#handle_dragover
        ; dragleaves super#root self#handle_dragleave
        ; drops super#root self#handle_drop
        ; dragends super#root self#handle_drag_end
        ]);
    super#initial_sync_with_dom ()

  method! destroy () : unit =
    List.iter Lwt.cancel _drag_listeners;
    _drag_listeners <- [];
    super#destroy ()

  method private handle_dragenter e _ =
    Dom_html.stopPropagation e;
    Dom.preventDefault e;
    _dragenter_target <- e##.target;
    ghost##.style##.display := Js.string "";
    Lwt.return_unit

  method private handle_dragover e _ =
    let a = Js.Unsafe.coerce e##.dataTransfer##.types in
    let l = Js.to_array a |> Array.to_list |> List.map Js.to_string in
    let rec find_loop = function
      | [] -> None
      | typ :: tl ->
        match split_string Resizable.drag_type_prefix typ with
        | None -> find_loop tl
        | Some "" -> Some (None, typ)
        | Some s ->
          match String.split_on_char '-' s with
          | "" :: data :: [] ->
            (match String.split_on_char ':' data with
             | w :: h :: [] ->
               (match int_of_string_opt w, int_of_string_opt h with
                | Some w, Some h -> Some (Some (w, h), typ)
                | _ -> find_loop tl)
             | _ -> find_loop tl)
          | _ -> find_loop tl
    in
    match find_loop l with
    | None -> Lwt.return_unit
    | Some (aspect, typ) ->
      _dnd_typ <- typ;
      self#move_ghost ?aspect e; Lwt.return_unit

  method private handle_dragleave e _ =
    Dom_html.stopPropagation e;
    Dom.preventDefault e;
    if _dragenter_target == e##.target
    then ghost##.style##.display := Js.string "none";
    Lwt.return_unit

  method private handle_drop e _ =
    Dom.preventDefault e;
    let json =
      try e##.dataTransfer##getData (Js.string _dnd_typ)
          |> Js.to_string
          |> Yojson.Safe.from_string
      with _ -> `Null in
    self#handle_dropped_json json
    >>= fun () ->
    ghost##.style##.display := Js.string "none";
    Lwt.return_unit

  method private handle_drag_end e _ =
    ghost##.style##.display := Js.string "none";
    Lwt.return_unit

  method private size : int * int =
    elt##.offsetWidth, elt##.offsetHeight

  method private virtual move_ghost : 'a. ?aspect:int * int
    -> (#Dom_html.event as 'a) Js.t
    -> unit

  method private virtual handle_dropped_json : Yojson.Safe.json -> unit Lwt.t

end
