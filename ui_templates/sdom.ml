open Containers
open Components

type ('a, 'b) setter =
  { get : 'a -> 'b
  ; eq : 'b -> 'b -> bool
  ; upd : 'b -> unit
  }

let setter ?(previous : 'a option) (model : 'a) (s : ('a,'b) setter) =
  match previous with
  | Some prev ->
     if s.eq (s.get prev) (s.get model)
     then s.upd @@ s.get model
  | None -> s.upd @@ s.get model

module type Node = sig

  type model
  type widget

  val equal_model : model -> model -> bool
  val make : model -> widget * (model -> unit)
  val widget : widget -> Widget.t

end

module type Array_node = sig

  include Node

  module Id : sig
    type t
    val of_string : string -> t option
    val to_string : t -> string
    val compare : t -> t -> int
  end

  val id_of_model : model -> Id.t

end

module type Array_root_node = sig

  module Node : Array_node

  type widget

  val make : Node.model list -> widget * (Node.model list -> unit)
  val root : widget -> Dom_html.element Js.t
  val append_child : widget -> Node.widget -> unit
  val insert_child_at_idx : widget -> int -> Node.widget -> unit
  val remove_child : widget -> Node.widget -> unit

end

module Make_array(M : Array_root_node) = struct

  type id = M.Node.Id.t
  type model = M.Node.model
  type widget = M.Node.widget

  type nodes =
    { mutable hidden : (widget * (model -> unit)) list
    ; mutable active : (widget * (model -> unit)) list
    }

  let id_of_widget (w : widget) =
    (M.Node.widget w)#get_attribute "data-id"
    |> Option.get_exn
    |> M.Node.Id.of_string
    |> Option.get_exn
  let id_to_widget (id : id) (w : widget) =
    let id = M.Node.Id.to_string id in
    (M.Node.widget w)#set_attribute "data-id" id

  let eq_id (a : model) (b : model) =
    match M.Node.Id.compare (M.Node.id_of_model a) (M.Node.id_of_model b) with
    | 0 -> true | _ -> false

  let find_opt (id : id) (l : widget list) : widget option =
    List.find_opt (fun x -> 0 = M.Node.Id.compare id @@ id_of_widget x) l

  let insert_before
        ~(parent : M.widget)
        (nodes : nodes)
        (widget, upd) =
    let c = List.find_idx (fun (x : widget) ->
                match M.Node.Id.compare
                        (id_of_widget x)
                        (id_of_widget widget) with
                | 1 -> true
                | _ -> false) @@ List.map fst nodes.active
            |> Option.map (fun (id, x) -> id, (M.Node.widget x)#root) in
    match c with
    | Some (id, _) ->
       M.insert_child_at_idx parent id widget;
       nodes.active <- List.insert_at_idx id (widget, upd) nodes.active
    | None ->
       M.append_child parent widget;
       nodes.active <- nodes.active @ [ widget, upd ]

  let partition (o : model list) (n : model list) =
    let lost =
      List.filter (fun x -> not @@ List.mem ~eq:eq_id x n) o in
    let found, changed =
      List.partition_map (fun x ->
          match List.find_opt (eq_id x) o with
          | Some i -> if M.Node.equal_model x i then `Drop else `Right x
          | None -> `Left x) n in
    lost, found, changed

  let handle_found parent (found : model list) (nodes : nodes) =
    let rec aux acc hidden = function
      | [ ] -> nodes.hidden <- hidden; acc
      | hd :: tl ->
         (match hidden with
          | [] ->
             let node = M.Node.make hd in
             id_to_widget (M.Node.id_of_model hd) (fst node);
             aux (node :: acc) [] tl
          | (w, upd) :: r ->
             id_to_widget (M.Node.id_of_model hd) w;
             upd hd;
             aux ((w, upd) :: acc) r tl) in
    let found = aux [] nodes.hidden found in
    List.iter (insert_before ~parent nodes) found

  let handle_lost (parent : M.widget) (lost : model list) (nodes : nodes) =
    List.iter (fun (x : model) ->
        let id = M.Node.id_of_model x in
        match List.find_idx (fun (w, _) ->
                  0 = M.Node.Id.compare id @@ id_of_widget w)
                nodes.active with
        | None -> ()
        | Some (idx, node) ->
           let active' = List.remove_at_idx idx nodes.active in
           nodes.active <- active';
           nodes.hidden <- node :: nodes.hidden;
           M.remove_child parent (fst node)) lost

  let handle_changed (changed : model list) (nodes : nodes) =
    List.iter (fun (x : model) ->
        let id = M.Node.id_of_model x in
        match List.find_opt (fun (w,_) ->
                  0 = M.Node.Id.compare id @@ id_of_widget w)
                nodes.active with
        | None -> ()
        | Some (_, upd) -> upd x) changed

  let make (init : model list) =
    let prev = ref [] in
    let (nodes : nodes) =
      { hidden = []
      ; active = [] } in
    let leaf, update' = M.make init in
    let update = fun model ->
      let lost, found, changed = partition !prev model in
      handle_found leaf found nodes;
      handle_lost  leaf lost  nodes;
      handle_changed changed nodes;
      update' model;
      prev := model
    in
    (* Initial call *)
    update init;
    leaf, update

end
