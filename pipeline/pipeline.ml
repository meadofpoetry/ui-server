open Pipeline_protocol
module Api_template = Api_cohttp_template.Make (Application_types.User)

(*
let connect_db streams_events dbs =
  Lwt_main.run @@ Storage.init dbs;
  E.map_s (fun s -> Storage.request dbs (Storage.Store_structures s)) streams_events
 *)
let typ = "pipeline"

let make_input_tab_template (cpu : Application_types.Topology.topo_cpu) =
  ( Netlib.Uri.Path.of_string cpu.process,
    Api_template.make_template_props ~title:"QoE"
      ~stylesheets:[ "/css/Chart.min.css"; "/css/pipeline.min.css" ]
      ~pre_scripts:
        [
          `Src "/js/moment.min.js";
          `Src "/js/Chart.min.js";
          `Src "/js/chartjs-plugin-streaming.min.js";
        ]
      ~post_scripts:[ `Src "/js/pipeline-page-input.js" ]
      () )

let create (cpu : Application_types.Topology.topo_cpu) kv db =
  let ( >>= ) = Lwt.bind in
  let ( >>=? ) = Lwt_result.bind in
  (*
  Kv.RW.parse ~default:Pipeline_settings.default Pipeline_settings.of_string kv ["pipeline";"settings"]
  >>=? fun cfg ->
 *)
  Pipeline_protocol.Protocol.create db kv >>=? fun state ->
  let reset state ss =
    Pipeline_protocol.Protocol.reset state ss >>= function
    | Ok () -> Lwt.return_unit
    | Error (`Qoe_backend e) -> Lwt.fail_with e
  in

  (*React.E.keep @@ connect_db (S.changes api.streams) dbs;*)
  Logs.info (fun m -> m "(Pipeline) created");
  Lwt.return_ok
  @@ object
       val state = state

       method reset ss = reset state ss

       method http = Pipeline_http.handlers state

       method ws = Pipeline_http.ws state

       method pages = Pipeline_http.pages ()

       method tabs =
         List.map (fun i -> (`Input i, [ make_input_tab_template cpu ]))
         @@ Application_types.Topology.topo_inputs_of_topo_cpu cpu

       method finalize () = Pipeline_protocol.Protocol.finalize state

       method log_source filter =
         let sf =
           Log_converters.Status.to_log_messages state.sources
             state.options.structures#s filter
         in
         (* TODO
            let vf =
              Log_converters.Video.to_log_messages state.sources state.options.structures#s filter
            in
            let af =
              Log_converters.Audio.to_log_messages state.sources state.options.structures#s filter
            in
         *)
         Util_react.E.aggregate_merge
           ~merge:(fun acc x -> x @ acc)
           (fun () -> Lwt_unix.sleep 1.0)
           [
             (*React.E.map vf state.notifs.vdata
               ; React.E.map af state.notifs.adata
               ; *)
             React.E.map sf state.notifs.status_raw;
           ]
     end
