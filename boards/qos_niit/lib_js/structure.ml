open Containers
open Common
open Components
open Board_types.Streams.TS
open Lwt_result.Infix

let split (s:(Stream.id * structure) list React.signal) =
  let open React in
  let eq  = Stream.equal_id in
  let acc =
    S.fold (fun acc x ->
        let rec aux a = function
          | []     -> a
          | x :: l -> aux (List.add_nodup ~eq x a) l in
        aux acc x)
      (List.map fst @@ S.value s) (S.changes @@ S.map (List.map fst) s) in
  let lost, found =
    S.diff (fun n o ->
        let lost  = List.(filter (fun (x,_) -> not @@ Assoc.mem ~eq x n) o) in
        let found = List.(filter (fun (x,_) -> not @@ Assoc.mem ~eq x o) n) in
        lost, found) s
    |> fun s -> E.map fst s, E.map snd s in
  let add =
    E.fmap (fun s ->
        let streams = React.S.value acc in
        List.filter (fun (x,_) -> not @@ List.mem ~eq x streams) s
        |> function [] -> None | l -> Some l)
      found in
  lost, found, add

let structs (factory:Widget_factory.t)
      (s:(Stream.id * structure) list React.signal) =
  let open Widget_factory in
  (* FIXME remove *)
  let s,push = React.S.create [] in
  let open Lwt.Infix in
  let _ = Api_js.Requests.Json_request.get
            ?scheme:None ?host:None ?port:None
            ~path:Uri.Path.Format.("/js/structure.json" @/ empty)
            ~query:Uri.Query.empty
          >|= (fun r ->
      Result.get_exn r
      |> Json.(List.of_yojson (Pair.of_yojson Stream.id_of_yojson structure_of_yojson))
      |> Result.get_exn
      |> push)
  in
  (* FIXME end of temp block *)
  let lost, found, to_add = split s in
  let box = new Vbox.t ~widgets:[] () in
  let add id =
    print_endline "adding";
    let i = factory#create (Structure (Some { stream = id })) in
    print_endline "created item";
    let p = new Expansion_panel.t
              ~title:"Структура"
              ~content:[i.widget] () in
    print_endline "created";
    Dom.appendChild box#root p#root;
    print_endline "added" in
  let () = React.E.map (fun l -> print_endline "adding e";
                                 List.iter Fun.(add % fst) l) found
           |> Lwt_react.E.keep in
  List.iter Fun.(add % fst) @@ React.S.value s;
  box#widget

let page control () =
  let factory = new Widget_factory.t control () in
  factory#structs
  >|= (structs factory)
  |> Ui_templates.Loader.create_widget_loader
  |> (fun w -> w#set_on_destroy (Some factory#destroy); w)
