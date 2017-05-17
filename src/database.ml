open Sqlite3

type database_settings = { path : string }
                       
let create settings =
  Sqlite3.db_open ~mode:`NO_CREATE settings.path
