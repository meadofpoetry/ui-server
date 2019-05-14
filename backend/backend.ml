let tz_offset_s = Ptime_clock.current_tz_offset_s ()

let rfc3339_adjust_tz_offset tz_offset_s =
  let min = -86340 (* -23h59 in secs *) in
  let max = +86340 (* +23h59 in secs *) in
  if min <= tz_offset_s && tz_offset_s <= max && tz_offset_s mod 60 = 0
  then tz_offset_s
  else 0 (* UTC *)

let pp_time ?tz_offset_s () ppf t =
  let tz_offset_s = match tz_offset_s with
    | Some tz -> rfc3339_adjust_tz_offset tz
    | None -> 0
  in
  let (y, m, d), ((hh, ss, mm), _) =
    Ptime.to_date_time ~tz_offset_s t in
  Format.fprintf ppf "[%04d-%02d-%02d %02d:%02d:%02d]" y m d hh ss mm

let lwt_reporter ppf =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    Fmt.with_buffer ~like b,
    fun () -> let m = Buffer.contents b in Buffer.reset b; m
  in
  let _, app_flush = buf_fmt ~like:Fmt.stdout in
  let _, dst_flush = buf_fmt ~like:Fmt.stderr in
  (* let reporter = Logs_fmt.reporter ~app ~dst () in *)
  let report src level ~over k msgf =
    let k _ =
      let write () = match level with
        | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
        | _ -> Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () = over (); Lwt.return_unit in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    let with_stamp h _ k ppf fmt =
      let dt = Ptime_clock.now () in
      let pp = pp_time ?tz_offset_s () in
      match Logs.Src.equal src Logs.default with
      | true ->
         Format.kfprintf k ppf ("%a %a @[" ^^ fmt ^^ "@]@.")
           pp dt Logs.pp_header (level, h)
      | false ->
         Format.kfprintf k ppf ("%a [%s] %a @[" ^^ fmt ^^ "@]@.")
           pp dt (Logs.Src.name src) Logs.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k ppf fmt
  in
  { Logs.report = report }

(*
module Api_handler = Api.Handler.Make(Common.User)

let tz_offset_s = Ptime_clock.current_tz_offset_s ()

let rfc3339_adjust_tz_offset tz_offset_s =
  let min = -86340 (* -23h59 in secs *) in
  let max = +86340 (* +23h59 in secs *) in
  if min <= tz_offset_s && tz_offset_s <= max && tz_offset_s mod 60 = 0
  then tz_offset_s
  else 0 (* UTC *)

let pp_time ?tz_offset_s () ppf t =
  let tz_offset_s = match tz_offset_s with
    | Some tz -> rfc3339_adjust_tz_offset tz
    | None -> 0
  in
  let (y, m, d), ((hh, ss, mm), _) =
    Ptime.to_date_time ~tz_offset_s t in
  Format.fprintf ppf "[%04d-%02d-%02d %02d:%02d:%02d]" y m d hh ss mm

let lwt_reporter ppf =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    Fmt.with_buffer ~like b,
    fun () -> let m = Buffer.contents b in Buffer.reset b; m
  in
  let _, app_flush = buf_fmt ~like:Fmt.stdout in
  let _, dst_flush = buf_fmt ~like:Fmt.stderr in
  (* let reporter = Logs_fmt.reporter ~app ~dst () in *)
  let report src level ~over k msgf =
    let k _ =
      let write () = match level with
        | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
        | _ -> Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () = over (); Lwt.return_unit in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    let with_stamp h _ k ppf fmt =
      let dt = Ptime_clock.now () in
      let pp = pp_time ?tz_offset_s () in
      match Logs.Src.equal src Logs.default with
      | true ->
         Format.kfprintf k ppf ("%a %a @[" ^^ fmt ^^ "@]@.")
           pp dt Logs.pp_header (level, h)
      | false ->
         Format.kfprintf k ppf ("%a [%s] %a @[" ^^ fmt ^^ "@]@.")
           pp dt (Logs.Src.name src) Logs.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k ppf fmt
  in
  { Logs.report = report }
  
let main log_level config =
  ignore (Nocrypto_entropy_lwt.initialize ());
  Logs.set_reporter (lwt_reporter (Format.std_formatter));
  Logs.set_level (Some log_level);
  
  let mainloop () =
    print_endline "Started.";
    (* State *)
    let db             = Storage.Database.create config (4. *. 3600.0) in (* 60 seconds is for debug purpose only, need a bigger interval *)
    let app, app_loop  = Application.create config db in
    let routes         = Api_handler.create @@ Application_api.handlers app in
    let templates      = Application_template.create app in
    let auth_filter    = Application.redirect_filter app in
    
    let server = Serv.create config auth_filter routes templates in

    let loops = [server; app_loop] in
    try
      Lwt_main.run @@ Lwt.pick loops;
    with
    | Failure s -> begin
        Printf.printf "Failed with msg: %s\nRestarting...\n" s;
        
        print_endline "done";
        List.iteri (fun i t ->
            match Lwt.state t with
            | Lwt.Sleep  -> Printf.printf "Thread %d is sleeping\n" i
            | Lwt.Fail e -> Printf.printf "Thread %d is failed with exn %s\n" i (Printexc.to_string e)
            | Lwt.Return _ -> Printf.printf "Thread %d is done\n" i) loops;
        
        Application.finalize app;
        Storage.Database.finalize db;
        (* mainloop () *)
      end

    | e -> print_endline (Printf.sprintf "failed with exn: %s" (Printexc.to_string e))
  in
  try mainloop ()
  with e -> Printf.printf "Error: %s\n%s\n" (Printexc.get_backtrace ()) (Printexc.to_string e)
 *)

let ( / ) = Filename.concat

let ( >>= ) = Lwt_result.bind

let unwrap = function Ok v -> v | Error e -> failwith e
            
(* TODO let operators *)
let main () =
  let config_path = Xdg.config_dir / "ui_server" in
  Lwt.return @@ Kv.RW.create ~create:true ~path:config_path >>= fun kv ->
  Kv.RW.parse ~default:Db_conf.default Db_conf.of_string kv ["db"] >>= fun db_conf ->
  (* TODO getenv opt *)
  Lwt.return @@ Db.create
    ~role:(Sys.getenv "USER")
    ~password:db_conf.password
    ~socket_path:db_conf.socket_path
    ~cleanup:db_conf.cleanup
    ~maintain:db_conf.cleanup
  >>= fun db ->

  Server.create_config kv
  >>= fun server_conf ->
  
  Application.create kv db
  >>= fun (app, app_loop) -> (* TODO move template to application *)
  
  Futil_lwt.File.read (unwrap @@ Futil.Path.of_string @@ Sys.argv.(1))
  >>= fun template ->

  let routes = Application_http.create template app in (* TODO proper template init *)
  let auth_filter = Application.redirect_filter app in
  
  let server = Server.create server_conf auth_filter routes in
  
  ignore db_conf;
  Lwt.bind (Lwt.pick [app_loop; server]) Lwt.return_ok

let () =
  let log_level =
    match Sys.getenv_opt "UI_LOG_LEVEL" with
    | Some "debug" -> Logs.Debug
    | Some "info" -> Logs.Info
    | Some "warning" -> Logs.Warning
    | Some "error" -> Logs.Error
    | _ -> Logs.Error
  in
  Lwt_engine.set
    ~transfer:true
    ~destroy:true
    (new Lwt_engine.libev ~backend:Lwt_engine.Ev_backend.epoll ());
  ignore (Nocrypto_entropy_lwt.initialize ());
  Logs.set_reporter (lwt_reporter (Format.std_formatter));
  Logs.set_level (Some log_level);

  if Array.length @@ Sys.argv <> 2
  then begin
      Printf.printf "Usage:\n\t%s [path to template]\n" Sys.argv.(0);
      exit (-1)
    end;
  
  match Lwt_main.run (main ()) with
  | Ok () ->
     print_endline "Ui Server is done, no error reported"
  | Error (#Kv.RO.error as e) ->
     Logs.err (fun m -> m "Terminated with file error: %a"
                          Kv.RO.pp_error e)
  | Error (#Kv.RW.parse_error as e) ->
     Logs.err (fun m -> m "Terminated with file read error: %a"
                          Kv.RW.pp_parse_error e)
  | Error (#Kv_v.error as e) ->
     Logs.err (fun m -> m "Terminated with config error: %a"
                          Kv_v.pp_error e)
  | Error (#Db.error as e) ->
     Logs.err (fun m -> m "Terminated with database error: %a"
                          Db.pp_error e)
  | Error (#Db.conn_error as e) ->
     Logs.err (fun m -> m "Terminated with database error: %a"
                          Db.pp_conn_error e)
  | Error (#Boards.Board.error as e) ->
     Logs.err (fun m -> m "Terminated with board error: %a"
                          Boards.Board.pp_error e)
  | Error (#Pc_control.Network.error as e) ->
     Logs.err (fun m -> m "Terminated with network error: %a"
                          Pc_control.Network.pp_error e)
