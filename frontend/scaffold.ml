open Js_of_ocaml
open Js_of_ocaml_lwt
open Components

let is_firefox () =
  let ua = Dom_html.window##.navigator##.userAgent in
  ua##search (new%js Js.regExp (Js.string "Firefox")) > -1

let logout' href =
  let open XmlHttpRequest in
  let req = create () in
  req##_open (Js.string "GET")
    (* meth *) (Js.string "/api/user/logout")
    Js._true
  (* async *);
  req##.onreadystatechange :=
    Js.wrap_callback (fun _ ->
        match req##.readyState with
        | DONE ->
            Js.Optdef.iter href (fun href ->
                Dom_html.window##.location##replace href)
        | _ -> ());
  req##send Js.null

let logout href =
  let open XmlHttpRequest in
  let req = create () in
  let location = Dom_html.window##.location in
  let password =
    Js.some @@ Js.string @@ string_of_float @@ (new%js Js.date_now)##getTime
  in
  req##_open_full
    (Js.string "GET") (* meth *)
    location##.href (* url *) Js._true (* async *)
    (Js.some @@ Js.string "logout") (*username *)
    password;
  req##.onreadystatechange :=
    Js.wrap_callback (fun _ ->
        match req##.readyState with
        | DONE ->
            Js.Optdef.iter href (fun href ->
                Dom_html.window##.location##replace href)
        | _ -> ());
  req##send Js.null

let () =
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  let logout = if is_firefox () then logout else logout' in
  Js.Unsafe.global##.scaffold := scaffold;
  Js.Unsafe.global##.logout := Js.wrap_callback logout
