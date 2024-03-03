let db_url = "./db/url_shortner.sqlite"
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
    "INSERT INTO %s (%s) VALUES (?);" db_table_name db_col_long_url in
  let insert_stmt = Sqlite3.prepare db stmt in
  let url_data = Sqlite3.Data.TEXT url in
  ignore (Sqlite3.bind insert_stmt 1 url_data);
  ignore (Sqlite3.step insert_stmt);
  let last_insert_id = Sqlite3.last_insert_rowid db in
  ignore (Sqlite3.finalize insert_stmt);
  ignore (Sqlite3.db_close db);
  Int64.to_int last_insert_id

let get_longurl (id: int) =
  let db = Sqlite3.db_open db_url in
  let stmt = Printf.sprintf
    "SELECT %s FROM %s WHERE id = %i;" db_col_long_url db_table_name id in
    let result = ref None in
    let _ = (Sqlite3.exec db ~cb:(fun row _ -> result := Some row) stmt) in
    match !result with
    | Some row -> row.(0) (* Assuming there's only one column in the result *)
    | None -> None
  