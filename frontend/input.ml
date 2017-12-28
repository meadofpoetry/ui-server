open Components

let () =
  let controls = Array.to_list @@ Js.to_array @@ Js.Unsafe.variable "boards" in
  print_endline @@ CCString.concat " " @@ CCList.map string_of_int controls
