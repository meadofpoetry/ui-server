open Application_types

include Qoe_backend_types.Wm.Make (Stream.ID)
  
let default : t = { resolution = 1280, 720
                  ; widgets   = []
                  ; layout    = []
                  }

module Annotated = struct

  type raw = t

  type raw_container = container

  type state = [`Active | `Stored ] [@@deriving yojson, eq]

  type container =
    { position : position
    ; widgets  : (string * state * widget) list
    } [@@deriving yojson, eq]

  type t =
    { resolution : int * int
    ; widgets    : (string * widget) list
    ; layout     : (string * state * container) list
    } [@@deriving yojson, eq]

  let opt_map f = function
    | None -> None
    | Some x -> Some (f x)
              
  (* TODO remove after 4.08 *)
  let rec filter_opt = function
    | [] -> []
    | None::tl -> filter_opt tl
    | (Some h)::tl -> h::(filter_opt tl)
    
  let opt_get_list opt ~deref =
    match opt with
    | None -> []
    | Some l -> deref l

  let match_and_filter ~pred ~main l =
    let rec find_and_filter pred = function
      | [] -> None, []
      | h::tl ->
         if pred h
         then Some h, tl
         else
           let res, tl' = find_and_filter pred tl in
           res, h::tl'
    in
    List.fold_left (fun (acc, l) el ->
        let r, rest = find_and_filter (pred el) l in
        (Some el, r)::acc, rest)
      ([], l)
      main
    
  let annotate ~(active : raw) ~(stored : raw) : t =

    let merge_widgets
          ?(active : (string * widget) option)
          ?(stored : (string * widget) option)
          () =

      match active, stored with
      | Some (id, w), _ -> Some (id, `Active, w)
      | _, Some (id, w) -> Some (id, `Stored, w)
      | _ -> None
    in
    
    let merge_containers
          ?(active : (string * raw_container) option)
          ?(stored : (string * raw_container) option)
          () =

      let active_widgs, stored_widgs' =
        match_and_filter
          ~pred:(fun (n1,_) (n2,_) -> String.equal n1 n2)
          ~main:(opt_get_list active ~deref:(fun (_,x) -> x.widgets))
          (opt_get_list stored ~deref:(fun (_,x) -> x.widgets))
      in
      let stored_widgs = List.map (fun x -> None, Some x) stored_widgs' in

      let widgets = filter_opt
                    @@ List.map (fun (active, stored) ->
                        merge_widgets ?active ?stored ())
                      (stored_widgs @ active_widgs)
      in
      match active, stored with
      | Some (id, c), _ -> Some (id, `Active, { position = c.position; widgets })
      | _, Some (id, c) -> Some (id, `Stored, { position = c.position; widgets })
      | _ -> None
            
    in

    let active_conts, stored_conts' = match_and_filter
                                        ~pred:(fun (n1,_) (n2,_) -> String.equal n1 n2)
                                        ~main:active.layout
                                        stored.layout
    in
    let stored_conts = List.map (fun x -> None, Some x) stored_conts' in

    let layout = filter_opt
                 @@ List.map
                   (fun (active, stored) -> merge_containers ?active ?stored ())
                   (stored_conts @ active_conts)
    in
    { resolution = active.resolution
    ; widgets = active.widgets
    ; layout
    }

  let update_stored ~(active : raw) ~(stored : raw) =
    let updated = ref false in

    let equal_widget (id1, w1 : (string * widget)) (id2, w2 : (string * widget)) =
      id1 = id2
      && w1.type_ = w2.type_
      && w1.domain = w2.domain
      && w1.pid = w2.pid
    in
    
    let for_each_widg
          ?(active_widget : (string * widget) option)
          ?(stored_widget : (string * widget) option)
          () =
      
      match active_widget, stored_widget with
      | None, None -> None
      | Some (id,w), _ -> Some (id,w)
      | _, Some (id,w) -> begin
          match List.find_opt (equal_widget (id,w)) active.widgets with
          | None -> None
          | Some _ ->
             updated := true;
             Some (id, w)
        end
      
    in
    
    let for_each_cont
          ?(active : (string * raw_container) option)
          ?(stored : (string * raw_container) option)
          () =

      let active_widgets, stored_widgets' =
        match_and_filter
          ~pred:equal_widget
          ~main:(opt_get_list active ~deref:(fun (_,x) -> x.widgets))
          (opt_get_list stored ~deref:(fun (_,x) -> x.widgets))
      in
      let stored_widgets = List.map (fun x -> None, Some x) stored_widgets' in

      let widgets =
        filter_opt
        @@ List.map (fun (active_widget, stored_widget) ->
               for_each_widg ?active_widget ?stored_widget ())
             (stored_widgets @ active_widgets)
      in

      match active, stored with
      | None, None -> None
      | Some (id,c), _ | _ , Some (id, c) ->
         if widgets = []
         then None
         else Some (id, { c with widgets })
         
    in

    let active_conts, stored_conts' = match_and_filter
                                        ~pred:(fun (n1,_) (n2,_) -> String.equal n1 n2)
                                        ~main:active.layout
                                        stored.layout
    in
    let stored_conts = List.map (fun x -> None, Some x) stored_conts' in
    
    let layout =
      filter_opt
      @@ List.map
           (fun (active, stored) -> for_each_cont ?active ?stored ())
           (stored_conts @ active_conts)
    in
    let res = { active with layout } in
    
    match !updated with
    | true -> `Changed res
    | false -> `Kept active

  let filter ~select (annotated : t) : raw =

    let rec filter_widgets = function
      | [] -> []
      | (id, state, widget)::tl ->
         if state <> select
         then filter_widgets tl
         else (id, widget)::(filter_widgets tl)
    in
    
    let rec filter_layout
            : (string * state * container) list -> (string * raw_container) list = function
      | [] -> []
      | (id, state, cont)::tl ->
         if state <> select
         then filter_layout tl
         else
           let widgets = filter_widgets cont.widgets in
           (id, {position = cont.position; widgets})::(filter_layout tl)
    in

    { resolution = annotated.resolution
    ; layout = filter_layout annotated.layout
    ; widgets = annotated.widgets
    }
    
end
