type t = string [@@deriving eq]

let empty = ""
       
let of_string s = s

let of_string_list sl =
  String.concat "/" sl
                               
let to_string p = p

let to_string_list p =
  String.split_on_char '/' p
                
let is_empty p =
  String.length p = 0
                               
let is_absolute p =
  try String.get p 0 = '/'
  with _ -> false

let is_absolute_ref p =
  try String.get p 0 = '/'
      && String.get p 1 = '/'
  with _ -> false

let make_absolute p =
  if is_absolute p
  then p
  else "/" ^ p

let make_absolute_ref p =
  if is_absolute_ref p
  then p
  else "//" ^ p
          
let concat pl =
  String.concat "/" pl
