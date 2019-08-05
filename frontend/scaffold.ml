open Js_of_ocaml
open Js_of_ocaml_lwt
open Components

let logout ?(reload = true) () =
  let open XmlHttpRequest in
  let req = create () in
  let location = Dom_html.window##.location in
  let password =
    Js.some
    @@ Js.string
    @@ string_of_float
    @@ (new%js Js.date_now)##getTime in
  req##_open_full
    (Js.string "HEAD") (* meth *)
    location##.href (* url *)
    Js._true (* async *)
    (Js.some @@ Js.string "logout") (*username *)
    password;
  req##.onreadystatechange := Js.wrap_callback (fun _ ->
      match req##.readyState with
      | DONE -> if reload then Dom_html.window##.location##reload
      | _ -> ());
  req##send Js.null

let () =
  let body = Dom_html.document##.body in
  let logout_selector = "a[href=\"/logout\"]" in
  let on_logout = fun _ _ -> logout ~reload:true (); Lwt.return_unit in
  let clicks =
    List.map (fun x -> Lwt_js_events.clicks x on_logout)
    @@ Element.query_selector_all body logout_selector in
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  scaffold#set_on_destroy (fun () -> List.iter Lwt.cancel clicks);
  Js.Unsafe.global##.scaffold := scaffold;
  Js.Unsafe.global##.logout := Js.wrap_callback (fun x ->
      logout ~reload:(Js.to_bool x) ())
