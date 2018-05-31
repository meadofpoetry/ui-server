(* File auto-generated by obus-gen-interface, DO NOT EDIT. *)
open OBus_value
open OBus_value.C
open OBus_member
open OBus_object
module Org_freedesktop_NetworkManager_Settings_Connection =
struct
  let interface = "org.freedesktop.NetworkManager.Settings.Connection"
  let m_ClearSecrets = {
    Method.interface = interface;
    Method.member = "ClearSecrets";
    Method.i_args = (arg0);
    Method.o_args = (arg0);
    Method.annotations = [];
  }
  let m_Delete = {
    Method.interface = interface;
    Method.member = "Delete";
    Method.i_args = (arg0);
    Method.o_args = (arg0);
    Method.annotations = [];
  }
  let m_GetSecrets = {
    Method.interface = interface;
    Method.member = "GetSecrets";
    Method.i_args = (arg1
                       (Some "setting_name", basic_string));
    Method.o_args = (arg1
                       (Some "secrets", dict string (dict string variant)));
    Method.annotations = [];
  }
  let m_GetSettings = {
    Method.interface = interface;
    Method.member = "GetSettings";
    Method.i_args = (arg0);
    Method.o_args = (arg1
                       (Some "settings", dict string (dict string variant)));
    Method.annotations = [];
  }
  let m_Save = {
    Method.interface = interface;
    Method.member = "Save";
    Method.i_args = (arg0);
    Method.o_args = (arg0);
    Method.annotations = [];
  }
  let m_Update = {
    Method.interface = interface;
    Method.member = "Update";
    Method.i_args = (arg1
                       (Some "properties", dict string (dict string variant)));
    Method.o_args = (arg0);
    Method.annotations = [];
  }
  let m_UpdateUnsaved = {
    Method.interface = interface;
    Method.member = "UpdateUnsaved";
    Method.i_args = (arg1
                       (Some "properties", dict string (dict string variant)));
    Method.o_args = (arg0);
    Method.annotations = [];
  }
  let s_PropertiesChanged = {
    Signal.interface = interface;
    Signal.member = "PropertiesChanged";
    Signal.args = (arg1
                       (Some "properties", dict string variant));
    Signal.annotations = [];
  }
  let s_Removed = {
    Signal.interface = interface;
    Signal.member = "Removed";
    Signal.args = (arg0);
    Signal.annotations = [];
  }
  let s_Updated = {
    Signal.interface = interface;
    Signal.member = "Updated";
    Signal.args = (arg0);
    Signal.annotations = [];
  }
  let p_Unsaved = {
    Property.interface = interface;
    Property.member = "Unsaved";
    Property.typ = basic_boolean;
    Property.access = Property.readable;
    Property.annotations = [];
  }
  type 'a members = {
    m_ClearSecrets : 'a OBus_object.t -> unit -> unit Lwt.t;
    m_Delete : 'a OBus_object.t -> unit -> unit Lwt.t;
    m_GetSecrets : 'a OBus_object.t -> string -> (string * (string * OBus_value.V.single) list) list Lwt.t;
    m_GetSettings : 'a OBus_object.t -> unit -> (string * (string * OBus_value.V.single) list) list Lwt.t;
    m_Save : 'a OBus_object.t -> unit -> unit Lwt.t;
    m_Update : 'a OBus_object.t -> (string * (string * OBus_value.V.single) list) list -> unit Lwt.t;
    m_UpdateUnsaved : 'a OBus_object.t -> (string * (string * OBus_value.V.single) list) list -> unit Lwt.t;
    p_Unsaved : 'a OBus_object.t -> bool React.signal;
  }
  let make members =
    OBus_object.make_interface_unsafe interface
      [
      ]
      [|
        method_info m_ClearSecrets members.m_ClearSecrets;
        method_info m_Delete members.m_Delete;
        method_info m_GetSecrets members.m_GetSecrets;
        method_info m_GetSettings members.m_GetSettings;
        method_info m_Save members.m_Save;
        method_info m_Update members.m_Update;
        method_info m_UpdateUnsaved members.m_UpdateUnsaved;
      |]
      [|
        signal_info s_PropertiesChanged;
        signal_info s_Removed;
        signal_info s_Updated;
      |]
      [|
        property_r_info p_Unsaved members.p_Unsaved;
      |]
end
