let db_url = "./db/url_shortner.sqlitedb"
let db_table_name = "urls"
let db_col_long_url = "long_url"

let bootstrap =
  ignore(print_endline "start bootstrapping DB");
  let db = Sqlite3.db_open db_url in
  let db_query_create_table =
    Printf.sprintf "CREATE TABLE %s
    (id INTEGER PRIMARY KEY AUTOINCREMENT, %s text NOT NULL);"
    db_table_name
    db_col_long_url in
  let init = ignore (Sqlite3.exec db db_query_create_table) in
  ignore(print_endline "finish bootstrapping DB");
  init

(* There are error chances at various DB steps and int conversion, I ignore them for now *)
let insert_into (url: string): int =
  let db = Sqlite3.db_open db_url in
  let stmt = Printf.sprintf
    "INSERT INTO %s (%s) VALUES (%s);" db_table_name db_col_long_url url in
  let last_insert_id = Sqlite3.last_insert_rowid db in
  ignore(Sqlite3.exec db stmt);
  Gc.full_major ();
  Int64.to_int last_insert_id
