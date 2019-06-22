open Js_of_ocaml
open Components

(* Inspired by https://github.com/nathancahill/split *)

module Style = struct
  let grid_template_columns = "grid-template-columns"
  let grid_template_rows = "grid-template-rows"

  let grid_column_gap = "grid-column-gap"
  let grid_row_gap = "grid-row-gap"
end

type direction = Col | Row

type event = Touch of Dom_html.touchEvent Js.t
           | Mouse of Dom_html.mouseEvent Js.t

let ( % ) f g x = f (g x)

let coerce_event = function
  | Touch e -> (e :> Dom_html.event Js.t)
  | Mouse e -> (e :> Dom_html.event Js.t)

(* FIXME merge with same function in `Resizable` module *)
let get_cursor_position ?touch_id = function
  | Mouse event ->
    begin match Js.Optdef.(to_option event##.pageX,
                           to_option event##.pageY) with
    | Some page_x, Some page_y -> page_x, page_y
    | _ -> failwith "no page coordinates in mouse event"
    end
  | Touch event ->
    let (e : Dom_html.touchEvent Js.t) = Js.Unsafe.coerce event in
    let touches = e##.changedTouches in
    let rec aux acc i =
      if i >= touches##.length then acc else
        let touch = Js.Optdef.get (touches##item i) (fun () -> assert false) in
        match touch_id with
        | None -> Some touch
        | Some id ->
          if touch##.identifier = id then Some touch else
            aux acc (succ i) in
    (match aux None 0 with
     | None -> failwith "no touch event found"
     | Some t -> t##.pageX, t##.pageY)

let split_string ~suffix pattern =
  let pattern_len = String.length pattern in
  let len = String.length suffix in
  if len > pattern_len
  then None
  else
    let sub = String.sub pattern (pattern_len - len) len in
    if String.uppercase_ascii sub = String.uppercase_ascii suffix
    then Some (String.sub pattern 0 (pattern_len - len))
    else None

type value =
  [ `Auto
  | `Px of float
  | `Fr of float
  | `Pc of float
  ] [@@deriving show]

let parse : string -> value option = function
  | "auto" -> Some `Auto
  | s ->
    let rec aux = function
      | [] -> None
      | suffix :: tl ->
        match split_string ~suffix s with
        | None -> aux tl
        | Some x ->
          match float_of_string_opt x with
          | None -> None
          | Some x ->
            match suffix with
            | "px" -> Some (`Px x)
            | "fr" -> Some (`Fr x)
            | "%" -> Some (`Pc x)
            | _ -> assert false
    in
    aux ["px"; "fr"; "%"]

let first_non_zero f a =
  let rec aux = function
    | n when n < 0 -> None
    | n ->
      match f a.(n) with
      | None -> aux (pred n)
      | Some v ->
        if v <> 0. then Some (n, v)
        else aux (pred n) in
  aux (pred @@ Array.length a)

class type stylish =
  object
    method style : Dom_html.cssStyleDeclaration Js.t Js.prop
  end

let get_matched_css_rules (elt : #Dom_html.element Js.t) : #stylish Js.t list =
  let stylesheets = (Js.Unsafe.coerce elt)##.ownerDocument##.styleSheets in
  let make_list coll =
    let rec aux acc = function
      | n when n < 0 -> acc
      | n ->
        let v = coll##item n in
        aux (v :: acc) (pred n) in
    aux [] coll##.length in
  List.filter (fun x ->
      try Element.matches elt (Js.to_string x##.selectorText)
      with _ -> false)
  @@ List.concat
  @@ List.map (fun x ->
      try make_list x##.rules
      with _ -> [])
  @@ make_list stylesheets

let get_styles (rule : string)
    (own : Dom_html.element Js.t list)
    (matched : #stylish Js.t list) =
  let rule = Js.string rule in
  let get_style x = Js.Unsafe.get x##.style rule in
  Utils.List.filter_map (fun (x : Js.js_string Js.t Js.optdef) ->
      Js.Optdef.case x
        (fun () -> None)
        (fun x -> match Js.to_string x with
           | "" -> None
           | s -> Some s))
  @@ (List.map get_style own) @ (List.map get_style matched)

type dimensions =
  { a_track : int
  ; b_track : int
  ; a_track_start : float
  ; percentage_to_pixels : float
  ; fr_to_pixels : float
  ; total_fr : int
  ; b_track_end : float
  ; tracks : string array
  ; track_values : value option array
  ; computed_values : value option array
  ; computed_gap : float
  ; drag_start_offset : float
  } [@@deriving show]

class t
    ?(drag_interval = 1)
    ?(snap_offset = 30.)
    ?(min_size_start = 0.)
    ?(min_size_end = 0.)
    ~(direction : direction)
    ~(track : int)
    (elt : Dom_html.element Js.t) () = object(self)
  inherit Widget.t elt () as super

  val mutable _grid = Js.null
  val mutable _listeners = []
  val mutable _move_listeners = []

  method! destroy () : unit =
    self#stop_move_listeners ();
    List.iter Lwt.cancel _listeners;
    _listeners <- [];
    super#destroy ()

  method! initial_sync_with_dom () : unit =
    _listeners <- Events.(
        [ mousedowns super#root (fun e t ->
              match e##.button with
              | 0 -> self#handle_drag_start (Mouse e) t
              | _ -> Lwt.return_unit)
        ; touchstarts super#root (fun e -> self#handle_drag_start (Touch e))
        ]);
    super#initial_sync_with_dom ()

  method private get_size_at_track ?(gap = 0) ?(end_ = false)
      track (tracks : value option array) =
    let tracks = Array.sub tracks 0 (if end_ then succ track else track) in
    let rec aux sum = function
      | n when n = Array.length tracks -> sum
      | n ->
        let sum = match tracks.(n) with
          | Some `Auto | None -> sum
          | Some (`Px x | `Fr x | `Pc x) -> sum +. x in
        aux sum (succ n) in
    (aux 0. 0) +. float_of_int (track * gap)

  method private get_cursor_position (e : event) =
    let position = get_cursor_position e in
    match direction with
    | Row -> float_of_int @@ snd position
    | Col -> float_of_int @@ fst position

  method private handle_drag_start (e : event) _ : unit Lwt.t =
    _grid <- Element.get_parent super#root;
    let start = Js.Opt.case _grid
        (fun () -> 0.)
        (fun grid -> match direction with
           | Row -> grid##getBoundingClientRect##.top
           | Col -> grid##getBoundingClientRect##.left) in

    let tracks = self#raw_tracks in
    let track_values = Array.map parse tracks in

    let computed_values = Array.map parse self#raw_computed_tracks in
    let fr_to_pixels = match first_non_zero (function
        | Some `Fr x -> Some x
        | _ -> None) track_values with
    | None -> 0.
    | Some (track, v) ->
      match computed_values.(track) with
      | Some (`Px x | `Fr x | `Pc x) -> x /. v
      | Some `Auto | None -> 0. in
    let percentage_to_pixels = match first_non_zero (function
        | Some `Pc x -> Some x
        | _ -> None) track_values with
    | None -> 0.
    | Some (track, v) ->
      match computed_values.(track) with
      | Some (`Px x | `Fr x | `Pc x) -> x /. v
      | Some `Auto | None -> 0. in

    let gutter_start =
      (self#get_size_at_track ~end_:false track computed_values)
      +. start in
    let a_track = pred track in
    let b_track =
      if track < Array.length tracks - 1
      then succ track
      else
        let error = Printf.sprintf
            "gutter: invalid track index (%d). \
             Track must be between two other tracks and only %d \
             tracks were found"
            track (Array.length tracks) in
        failwith error in
    let a_track_start =
      self#get_size_at_track ~end_:false a_track computed_values
      +. start in
    let b_track_end =
      self#get_size_at_track ~end_:true b_track computed_values
      +. start in
    let dimensions =
      { a_track
      ; b_track
      ; a_track_start
      ; b_track_end
      ; drag_start_offset = self#get_cursor_position e -. gutter_start
      ; total_fr = Array.fold_left (fun acc -> function
            | Some `Fr _ -> succ acc
            | _ -> acc) 0 track_values
      ; percentage_to_pixels
      ; fr_to_pixels
      ; tracks
      ; track_values
      ; computed_values
      ; computed_gap = match self#raw_computed_gap with
          | None -> 0.
          | Some x -> match parse x with
            | Some `Px x -> x
            | _ -> 0.
      } in
    (match e with
     | Touch e ->
       Dom.preventDefault e;
       _move_listeners <- Events.(
           [ touchmoves Dom_html.window (fun e ->
                 self#handle_drag dimensions (Touch e))
           ; touchends Dom_html.window (fun e -> self#handle_drag_stop (Touch e))
           ; touchcancels Dom_html.window (fun e -> self#handle_drag_stop (Touch e))
           ])
     | Mouse e ->
       Dom.preventDefault e;
       _move_listeners <- Events.(
           [ mousemoves Dom_html.window (fun e ->
                 self#handle_drag dimensions (Mouse e))
           ; mouseups Dom_html.window (fun e -> self#handle_drag_stop (Mouse e))
           ]));
    print_endline @@ show_dimensions dimensions;
    Lwt.return_unit

  method private handle_drag dimensions (e : event) _ : unit Lwt.t =
    let position = self#get_cursor_position e in
    let gutter_size = match dimensions.computed_values.(track) with
      | Some (`Px x | `Fr x | `Pc x) -> x
      | Some `Auto | None -> 0. in
    let min_position =
      dimensions.a_track_start
      +. min_size_start
      +. dimensions.drag_start_offset
      +. dimensions.computed_gap in
    let max_position =
      dimensions.b_track_end
      -. min_size_end
      -. dimensions.computed_gap
      -. (gutter_size -. dimensions.drag_start_offset) in
    let min_position_offset = min_position +. snap_offset in
    let max_position_offset = max_position -. snap_offset in
    let position =
      position
      |> (fun x -> if x < min_position_offset then min_position else x)
      |> (fun x -> if x > max_position_offset then max_position else x)
      |> max min_position
      |> min max_position in
    let a_track_size =
      position
      -. dimensions.a_track_start
      -. dimensions.drag_start_offset
      -. dimensions.computed_gap in
    let b_track_size =
      dimensions.b_track_end
      -. position
      +. dimensions.drag_start_offset
      -. gutter_size
      -. dimensions.computed_gap in
    let a_track_size, b_track_size =
      if drag_interval > 1
      then
        let interval = float_of_int drag_interval in
        let a_track_size_interleaved =
          Js.math##round (a_track_size /. interval) *. interval in
        a_track_size_interleaved,
        b_track_size -. a_track_size_interleaved -. a_track_size
      else a_track_size, b_track_size in
    let a_track_size = max a_track_size min_size_start in
    let b_track_size = max b_track_size min_size_end in
    self#adjust_position ~a_track_size ~b_track_size dimensions;
    Lwt.return_unit

  method private handle_drag_stop (e : event) _ : unit Lwt.t =
    self#stop_move_listeners ();
    Lwt.return_unit

  method private stop_move_listeners () : unit =
    List.iter Lwt.cancel _move_listeners;
    _move_listeners <- []

  method private adjust_position
      ~(a_track_size : float)
      ~(b_track_size : float)
      (dimensions : dimensions) =
    let tracks = dimensions.tracks in
    let update_track track track_size =
      match dimensions.track_values.(track) with
      | Some `Px x -> tracks.(track) <- Printf.sprintf "%gpx" a_track_size
      | Some `Fr x ->
        if dimensions.total_fr = 1
        then tracks.(track) <- "1fr"
        else tracks.(track) <- Printf.sprintf "%gfr"
              (track_size /. dimensions.fr_to_pixels)
      | Some `Pc x ->
        tracks.(track) <- Printf.sprintf "%g%%"
            (track_size /. dimensions.percentage_to_pixels)
      | _ -> () in
    update_track dimensions.a_track a_track_size;
    update_track dimensions.b_track b_track_size;
    let style = (String.concat " " @@ Array.to_list tracks) in
    self#set_style style

  method private set_style (style : string) : unit =
    Js.Opt.iter _grid (fun grid ->
        let v = Js.string style in
        match direction with
        | Row -> (Js.Unsafe.coerce grid##.style)##.gridTemplateRows := v
        | Col -> (Js.Unsafe.coerce grid##.style)##.gridTemplateColumns := v)

  method private raw_tracks : string array =
    let prop = match direction with
      | Col -> Style.grid_template_columns
      | Row -> Style.grid_template_rows in
    let elements, matched = Js.Opt.case _grid
        (fun () -> [], [])
        (fun x -> [x], get_matched_css_rules x) in
    let tracks = get_styles prop elements matched in
    if List.length tracks = 0
    then failwith "gutter: unable to determine grid template tracks from styles"
    else Array.of_list @@ String.split_on_char ' ' @@ List.hd tracks

  method private raw_computed_tracks : string array =
    Js.Opt.case _grid
      (fun () -> [||])
      (fun grid ->
         let style = Dom_html.window##getComputedStyle grid in
         let v = match direction with
           | Row -> (Js.Unsafe.coerce style)##.gridTemplateRows
           | Col -> (Js.Unsafe.coerce style)##.gridTemplateColumns in
         Js.Optdef.case v
           (fun () -> [||])
           (Array.of_list % String.split_on_char ' ' % Js.to_string))

  method private raw_computed_gap : string option =
    Js.Opt.case _grid
      (fun () -> None)
      (fun grid ->
         let style = Dom_html.window##getComputedStyle grid in
         let v = match direction with
           | Row -> (Js.Unsafe.coerce style)##.gridRowGap
           | Col -> (Js.Unsafe.coerce style)##.gridColumnGap in
         Js.Optdef.case v
           (fun () -> None)
           (fun x -> Some (Js.to_string x)))

end

let make ?drag_interval
    ?snap_offset
    ?min_size_start
    ?min_size_end
    ~(track : int)
    (direction : direction) =
  let elt = Dom_html.(createDiv document) in
  new t ?drag_interval ?snap_offset
    ?min_size_start
    ?min_size_end
    ~direction
    ~track
    elt
    ()
