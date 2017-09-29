include Cstruct

let get_ptr cs =
  Ctypes.(bigarray_start array1 cs.buffer)

exception Not_equal
  
let split_by_string on cs =
  let module BA = Bigarray.Array1 in
  
  let onsz  = CCString.length on in
  let bufsz = cs.len in
  if onsz >= bufsz
  then []
  else

    let rec lst_to_pairs = function
      | [] -> []
      | [_] -> failwith "odd number of elements"
      | x::y::xs -> (x,y)::(lst_to_pairs xs)
    in

    let rec find_subs i acc =
      if i < 0 then acc
      else try CCString.iteri
                 (fun j c ->
                   if c <> (Cstruct.get_char cs (i + j))
                   then raise_notrace Not_equal)
                 on;
               find_subs (i - onsz) (i::(i+onsz)::acc)
           with Not_equal -> find_subs (i - 1) acc
    in

    let points =
      ([0] @ (find_subs (bufsz - onsz) []) @ [bufsz])
      |> lst_to_pairs |> List.filter (fun (x,y) -> x <> y)
    in
    match points with
    | [] -> [cs]
    | _  ->
       List.map
         (fun (s,e) -> sub cs s (e - s))
         points

let split_size size cs =
  let module BA = Bigarray.Array1 in

  let bufsz = cs.len in
  let rec split point acc =
    if bufsz - point < size
    then
      let sz = (bufsz - point) in
      let r  = sub cs point sz in
      r::acc
    else
      let sz = size in
      let r  = sub cs point sz in
      split (point+size) (r::acc)
      
  in List.rev @@ split 0 []

let modify f cs =
  let module BA = Bigarray.Array1 in
  for i = cs.off to cs.len do
    BA.unsafe_set cs.buffer i @@ f @@ BA.unsafe_get cs.buffer i
  done

let modifyi f cs =
  let module BA = Bigarray.Array1 in
  for i = cs.off to cs.len do
    BA.unsafe_set cs.buffer i @@ f i @@ BA.unsafe_get cs.buffer i
  done

let pp ?(line_sz=16) b =
  let iter = Cstruct.iter (fun _ -> Some 1)
                          (fun buf -> Cstruct.get_uint8 buf 0)
                          b in
  let s = Cstruct.fold (fun acc el -> acc ^ (Printf.sprintf "%02x " el)) iter "" in
  let rec f = fun acc x -> if String.length x < (line_sz*16)
                           then List.rev (x :: acc)
                           else let s,res = CCString.take_drop (line_sz*16) x in
                                f (s :: acc) res in
  List.map (fun x -> x ^ "\n") (f [] s)
  |> String.concat ""
