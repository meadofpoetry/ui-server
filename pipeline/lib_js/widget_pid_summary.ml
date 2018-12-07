open Containers
open Components
open Qoe_status
open Common

let base_class = "pipeline-pids-summary"

module Set = Set.Make(Qoe_status)

module Pids = struct

  let _class = Markup.CSS.add_element base_class "pids"
  let pid_class = Markup.CSS.add_element _class "pid"
  let title_class = Markup.CSS.add_element _class "title"
  let box_class = Markup.CSS.add_element _class "wrapper"
  let lost_class = Markup.CSS.add_modifier pid_class "lost"

  class pid ?(hex = false) (state : t) () =
  object(self)

    inherit Widget.t Js_of_ocaml.Dom_html.(createSpan document) () as super

    method init () : unit =
      super#init ();
      self#add_class pid_class

    method update (x : t) =
      self#add_or_remove_class x.playing lost_class

    method pid : int =
      state.pid

  end

  let make_pid ?hex state =
    new pid ?hex state ()

  let make_title num =
    Printf.sprintf "PIDs (%d)" num

  class t ?hex (init : Qoe_status.t list) () =
    let text = make_title @@ List.length init in
    let title = new Typography.Text.t ~font:Caption ~text () in
    let pids_box = new Hbox.t ~widgets:[] () in
    object(self)

      val mutable _data : Set.t = Set.of_list init
      val mutable _pids : pid list = []

      inherit Vbox.t
                ~widgets:[ title#widget
                         ; pids_box#widget ] () as super

      method init () : unit =
        super#init ();
        _pids <- List.map (make_pid ?hex) init;
        List.iter pids_box#append_child _pids;
        self#add_class base_class;
        title#add_class title_class;
        pids_box#add_class box_class

      method destroy () : unit =
        super#destroy ();
        title#destroy ();
        pids_box#destroy ()

      method update (data : Qoe_status.t list) : unit =
        let prev = _data in
        _data <- Set.of_list data;
        let lost = Set.diff prev _data in
        let found = Set.diff _data prev in
        let upd = Set.inter prev _data in
        Set.iter self#remove_pid lost;
        Set.iter self#add_pid found;
        Set.iter self#update_pid upd;
        title#set_text @@ make_title @@ List.length _pids

      (* Private methods *)

      method private update_pid (x : Qoe_status.t) : unit =
        match List.find_opt (fun cell ->
                  cell#pid = x.pid) _pids with
        | None -> ()
        | Some cell -> cell#update x

      method private add_pid (x : Qoe_status.t) : unit =
        let pid = make_pid x in
        _pids <- pid :: _pids;
        pids_box#append_child pid

      method private remove_pid (x : Qoe_status.t) : unit =
        match List.find_opt (fun cell ->
                  cell#pid = x.pid) _pids with
        | None -> ()
        | Some cell ->
           pids_box#remove_child cell;
           cell#destroy ();
           _pids <- List.remove ~eq:Widget.equal cell _pids

    end

end

let split_services (x : Qoe_status.t list) : (int * Qoe_status.t) list =
  List.fold_left (fun acc (x : Qoe_status.t) ->
      List.Assoc.set ~eq:(=) x.channel x acc) [] x
