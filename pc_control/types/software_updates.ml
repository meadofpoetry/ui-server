type status =
  | Unknown
  | Wait
  | Setup
  | Running
  | Query
  | Info
  | Remove
  | Refresh_cache
  | Download
  | Install
  | Update
  | Cleanup
  | Obsolete
  | Dep_resolve
  | Sig_check
  | Test_commit
  | Commit
  | Request
  | Finished
  | Cancel
  | Download_repository
  | Download_packagelist
  | Download_filelist
  | Download_changelog
  | Download_group
  | Download_updateinfo
  | Repackaging
  | Loading_cache
  | Scan_applications
  | Generate_package_list
  | Waiting_for_lock
  | Waiting_for_auth
  | Scan_process_list
  | Check_executable_files
  | Check_libraries
  | Copy_files
  | Run_hook
  [@@deriving yojson]

let status_of_int32 = function
  | 0l -> Unknown
  | 1l -> Wait
  | 2l -> Setup
  | 3l -> Running
  | 4l -> Query
  | 5l -> Info
  | 6l ->  Remove
  | 7l ->  Refresh_cache
  | 8l ->  Download
  | 9l ->  Install
  | 10l ->  Update
  | 11l ->  Cleanup
  | 12l ->  Obsolete
  | 13l ->  Dep_resolve
  | 14l ->  Sig_check
  | 15l ->  Test_commit
  | 16l ->  Commit
  | 17l ->  Request
  | 18l ->  Finished
  | 19l ->  Cancel
  | 20l ->  Download_repository
  | 21l ->  Download_packagelist
  | 22l ->  Download_filelist
  | 23l ->  Download_changelog
  | 24l ->  Download_group
  | 25l ->  Download_updateinfo
  | 26l ->  Repackaging
  | 27l ->  Loading_cache
  | 28l ->  Scan_applications
  | 29l ->  Generate_package_list
  | 30l ->  Waiting_for_lock
  | 31l ->  Waiting_for_auth
  | 32l ->  Scan_process_list
  | 33l ->  Check_executable_files
  | 34l ->  Check_libraries
  | 35l ->  Copy_files
  | 36l ->  Run_hook
  | _ -> Unknown

let int32_of_status = function
  | Unknown -> 0l
  | Wait -> 1l
  | Setup -> 2l
  | Running -> 3l
  | Query -> 4l
  | Info -> 5l
  | Remove -> 6l
  | Refresh_cache -> 7l
  | Download -> 8l
  | Install -> 9l
  | Update -> 10l
  | Cleanup -> 11l
  | Obsolete -> 12l
  | Dep_resolve -> 13l
  | Sig_check -> 14l
  | Test_commit -> 15l
  | Commit -> 16l
  | Request -> 17l
  | Finished -> 18l
  | Cancel -> 19l
  | Download_repository -> 20l
  | Download_packagelist -> 21l
  | Download_filelist -> 22l
  | Download_changelog -> 23l
  | Download_group -> 24l
  | Download_updateinfo -> 25l
  | Repackaging -> 26l
  | Loading_cache -> 27l
  | Scan_applications -> 28l
  | Generate_package_list -> 29l
  | Waiting_for_lock -> 30l
  | Waiting_for_auth -> 31l
  | Scan_process_list -> 32l
  | Check_executable_files -> 33l
  | Check_libraries -> 34l
  | Copy_files -> 35l
  | Run_hook -> 36l

let status_equal : status -> status -> bool = Stdlib.(=)
              
type progress = int32 [@@deriving yojson]
              
type state = [ `Unchecked
             | `Checking of status * progress
             | `Updates_avail
             | `Upgrading of status * progress
             | `Need_reboot
             ] [@@deriving yojson]
