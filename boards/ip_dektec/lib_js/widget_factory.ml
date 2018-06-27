open Containers
open Components
open Board_types
open Lwt_result

type item =
  | Parameter of Widget_parameter.config [@@deriving yojson]

(* Widget factory *)
class t (control:int) () =
object(self)
  val mutable _state  = None
  val mutable _config = None
  val mutable _status = None

  val mutable _state_ref  = 0
  val mutable _config_ref = 0
  val mutable _status_ref = 0

  (** Create widget of type **)
  method create : item -> Dashboard.Item.item = function
    | _ -> { widget   = Dom_html.createDiv Dom_html.document |> Widget.create
           ; settings = None
           ; name     = ""
           }
    (* | Parameter conf -> Widget_parameter.make self#get_status conf *)
  method destroy () = (* self#destroy_status; _status_ref <- 0 *) ()
  method available : Dashboard.available = `List [ ]
  method serialize (x:item) : Yojson.Safe.json = item_to_yojson x
  method deserialize (json:Yojson.Safe.json) : (item,string) result = item_of_yojson json

  (* method private destroy_status =
   *   self#decr_status_ref;
   *   if _status_ref <= 0
   *   then (Option.iter (fun (_,f) -> f ()) _status; _status <- None)
   * 
   * method private incr_status_ref = _status_ref <- _status_ref + 1
   * method private decr_status_ref = _status_ref <- _status_ref - 1 *)

  (* method private get_status = match _status with
   *   | Some x -> self#incr_status_ref; fst x
   *   | None   -> _status_ref <- 1;
   *               let e,sock = Requests.get_status_ws control in
   *               let fin () = sock##close; React.E.stop ~strong:true e in
   *               _status <- Some (e,fin);
   *               e *)
end
