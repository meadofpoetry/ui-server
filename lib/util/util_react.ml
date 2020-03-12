include (
  Lwt_react :
    module type of struct
      include Lwt_react
    end
    with module E := Lwt_react.E
    with module S := Lwt_react.S )

type step = React.step

module E = struct
  include Lwt_react.E

  let next ev =
    let t, w = Lwt.task () in
    let ev = map (fun x -> Lwt.wakeup_later w x) (once ev) in
    Lwt.on_cancel t (fun () -> stop ev);
    t

  let changes ~(eq : 'a -> 'a -> bool) (e : 'a event) : 'a event = changes ~eq e

  let aggregate_merge ~merge t es =
    let merged = React.E.merge merge [] es in
    let tm = ref Lwt.return_unit in
    let result = ref [] in
    let event, epush = React.E.create () in
    let iter =
      React.E.fmap
        (fun l ->
          if Lwt.is_sleeping !tm then (
            result := l @ !result;
            None )
          else (
            tm := t ();
            result := l @ !result;
            Lwt.on_success !tm (fun () ->
                epush !result;
                result := []);
            None ))
        merged
    in
    React.E.select [ iter; event ]

  let aggregate t es = aggregate_merge ~merge:(fun acc x -> x :: acc) t es
end

module S = struct
  include Lwt_react.S

  let create ~eq = create ~eq

  let equal ~eq = equal ~eq

  let hold ~eq = hold ~eq

  let app ~eq = app ~eq

  let map ~eq = map ~eq

  let filter ~eq = filter ~eq

  let fmap ~eq = fmap ~eq

  let on ~eq = on ~eq

  let when_ ~eq = when_ ~eq

  let dismiss ~eq = dismiss ~eq

  let accum ~eq = accum ~eq

  let fold ~eq = fold ~eq

  let merge ~eq = merge ~eq

  let switch ~eq = switch ~eq

  let fix ~eq = fix ~eq

  let l1 ~eq f = l1 ~eq f

  let l2 ~eq f = l2 ~eq f

  let l3 ~eq f = l3 ~eq f

  let l4 ~eq f = l4 ~eq f

  let l5 ~eq f = l5 ~eq f

  let l6 ~eq f = l6 ~eq f

  (* Lwt react *)

  let bind ~eq = bind ~eq

  let bind_s ~eq = bind_s ~eq

  let limit ~eq = limit ~eq

  let app_s ~eq = app_s ~eq

  let map_s ~eq = map_s ~eq

  let filter_s ~eq = filter_s ~eq

  let fmap_s ~eq = fmap_s ~eq

  let accum_s ~eq = accum_s ~eq

  let fold_s ~eq = fold_s ~eq

  let merge_s ~eq = merge_s ~eq

  let l1_s ~eq = l1_s ~eq

  let l2_s ~eq = l2_s ~eq

  let l3_s ~eq = l3_s ~eq

  let l4_s ~eq = l4_s ~eq

  let l5_s ~eq = l5_s ~eq

  let l6_s ~eq = l6_s ~eq

  let run_s ~eq = run_s ~eq
end
