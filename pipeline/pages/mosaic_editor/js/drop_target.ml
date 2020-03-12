open Js_of_ocaml

let ( >>= ) = Lwt.bind

class virtual t (elt : Dom_html.element Js.t) () =
  object (self)
    inherit Components.Widget.t elt () as super

    val virtual ghost : Dom_html.element Js.t

    val virtual mutable format : string

    val mutable dnd_typ = ""

    val mutable dragenter_target = Js.null

    val mutable drag_listeners = []

    method! initial_sync_with_dom () : unit =
      drag_listeners <-
        Js_of_ocaml_lwt.Lwt_js_events.
          [
            dragenters super#root self#handle_dragenter;
            dragovers super#root self#handle_dragover;
            dragleaves super#root self#handle_dragleave;
            drops super#root self#handle_drop;
            dragends super#root self#handle_drag_end;
          ];
      super#initial_sync_with_dom ()

    method! destroy () : unit =
      List.iter Lwt.cancel drag_listeners;
      drag_listeners <- [];
      super#destroy ()

    method private handle_dragenter e _ =
      Dom_html.stopPropagation e;
      Dom.preventDefault e;
      dragenter_target <- e##.target;
      ghost##.style##.display := Js.string "";
      Lwt.return_unit

    method private handle_dragover e _ =
      let a = Js.Unsafe.coerce e##.dataTransfer##.types in
      let l = Js.to_array a |> Array.to_list |> List.map Js.to_string in
      match List.find_opt (String.equal format) l with
      | None ->
          print_endline "not found";
          Lwt.return_unit
      | Some typ ->
          dnd_typ <- typ;
          self#move_ghost e;
          Lwt.return_unit

    method private handle_dragleave e _ =
      Dom_html.stopPropagation e;
      Dom.preventDefault e;
      if dragenter_target == e##.target then
        ghost##.style##.display := Js.string "none";
      Lwt.return_unit

    method private handle_drop e _ =
      Dom.preventDefault e;
      let json =
        try
          e##.dataTransfer##getData (Js.string dnd_typ)
          |> Js.to_string
          |> Yojson.Safe.from_string
        with _ -> `Null
      in
      self#handle_dropped_json json >>= fun () ->
      ghost##.style##.display := Js.string "none";
      Lwt.return_unit

    method private handle_drag_end _ _ =
      ghost##.style##.display := Js.string "none";
      Lwt.return_unit

    method virtual private move_ghost
        : 'a. ?aspect:int * int -> (#Dom_html.event as 'a) Js.t -> unit

    method virtual private handle_dropped_json : Yojson.Safe.t -> unit Lwt.t
  end
