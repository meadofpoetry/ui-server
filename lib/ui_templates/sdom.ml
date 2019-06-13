open Components

type ('a, 'b) setter =
  { get : 'a -> 'b
  ; eq : 'b -> 'b -> bool
  ; upd : 'b -> unit
  }

let find_mapi f l =
  let rec aux f i = function
    | [] -> None
    | x::l' ->
      match f i x with
        | Some _ as res -> res
        | None -> aux f (i+1) l'
  in aux f 0 l

let find_idx p l = find_mapi (fun i x -> if p x then Some (i, x) else None) l

let insert_at_idx i x l =
  let rec aux l acc i x = match l with
    | [] -> List.rev_append acc [x]
    | y::l' when i=0 -> List.rev_append acc (x::y::l')
    | y::l' ->
      aux l' (y::acc) (i-1) x
  in
  let i = if i<0 then List.length l + i else i in
  aux l [] i x

let remove_at_idx i l0 =
  let rec aux l acc i = match l with
    | [] -> l0
    | _::l' when i=0 -> List.rev_append acc l'
    | y::l' ->
      aux l' (y::acc) (i-1)
  in
  let i = if i<0 then List.length l0 + i else i in
  aux l0 [] i

let partition_map f l =
  let rec iter f l1 l2 l = match l with
    | [] -> List.rev l1, List.rev l2
    | x :: tl ->
      match f x with
        | `Left y -> iter f (y :: l1) l2 tl
        | `Right y -> iter f l1 (y :: l2) tl
        | `Drop -> iter f l1 l2 tl
  in
  iter f [] [] l

(* TODO remove after 4.08 *)
module Option = struct
  let map f = function Some x -> Some (f x) | None -> None
  let get = function Some x -> x | None -> invalid_arg "value is None"
end

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
  val root : widget -> Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t
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
    |> Option.get
    |> M.Node.Id.of_string
    |> Option.get
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
    let c = find_idx (fun (x : widget) ->
        match M.Node.Id.compare
                (id_of_widget x)
                (id_of_widget widget) with
        | 1 -> true
        | _ -> false) @@ List.map fst nodes.active
            |> Option.map (fun (id, x) -> id, (M.Node.widget x)#root) in
    match c with
    | Some (id, _) ->
       M.insert_child_at_idx parent id widget;
       nodes.active <- insert_at_idx id (widget, upd) nodes.active
    | None ->
       M.append_child parent widget;
       nodes.active <- nodes.active @ [ widget, upd ]

  let partition (o : model list) (n : model list) =
    let lost =
      List.filter (fun x -> not @@ List.exists (eq_id x) n) o in
    let found, changed =
      partition_map (fun x ->
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
        match find_idx (fun (w, _) ->
                  0 = M.Node.Id.compare id @@ id_of_widget w)
                nodes.active with
        | None -> ()
        | Some (idx, node) ->
           let active' = remove_at_idx idx nodes.active in
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
