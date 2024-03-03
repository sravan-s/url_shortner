(*
  https://stackoverflow.com/questions/742013/how-do-i-create-a-url-shortener
   (1) Save URL in database
   (2) Get unique row ID for that URL from database
   (3) Convert integer ID to short string with encode(), e.g. 273984 to f5a4
   (4) Use the short string (e.g. f4a4) in your sharable URLs
   (5) When receiving a request for a short string (e.g. 20a8), decode the string to an integer ID with decode()
   (6) Look up URL in database for given ID
*)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

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

type create_payload = {
  url : string;
} [@@deriving yojson]

(*
0  → a
1  → b
...
25 → z
26 → A
...
52 → 0
61 → 9
*)
let alphabets = 
  ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 
   'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']

let seed_len = List.length alphabets

let rec encodenNum ?(accum: int list = []) n =
  let quotient = n / seed_len in
  let modulo = n mod seed_len in
  let new_list = (modulo::accum) in
    if quotient > 0 then
      encodenNum ~accum:new_list quotient
    else
      new_list

let cl2s cl = String.concat "" (List.map (String.make 1) cl)

let to_char li = cl2s @@ List.map (fun item -> (List.nth alphabets item)) li

let idx_in_seed (n: char) =
  let rec find_index_helper lst x index =
    match lst with
    | [] -> failwith "Invalid character"
    | hd :: t1 ->
      if hd = x then
        index
      else
        find_index_helper t1 x (index + 1)
  in find_index_helper alphabets n 0


(* [1, 33, 7] = `(1 * 24^2) + (33*24^1) + (7*24^0) *)
let int_list_to_num (li: int list): int =
  let folder_fn (i: int) ((total, pos): (int * int)): (int * int) =
    ((total + (i * int_of_float(float_of_int(seed_len) ** float_of_int(pos)))), (pos + 1)) in
  let (total, _) = List.fold_right folder_fn li (0, 0) in
  total

let string_to_char_list str =
  let char_seq = String.to_seq str in
  List.of_seq char_seq
let decode (li: string) =
  List.map idx_in_seed (string_to_char_list li)
  |> int_list_to_num

(* let _ = insert_into("asdassdas") *)

(* let l = to_char(encodenNum 2343)
let () = print_string l
let () = print_newline ()
let () = try
    print_int (decode l)
    with
    | Failure msg -> print_endline (msg);
    |  _ -> print_endline "Unknown error occurred" *)

let _ = bootstrap

(* www.wikipedia.com/a/b/c => Link.lasso/v *)
let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [

    Dream.get "/"
      (fun _ ->
        Dream.html ~status:`Accepted ~code:204 "Good morning, world!");

    Dream.post "/shorten"
      (fun request ->
        let%lwt body = Dream.body request in

        let create_payload =
          body
          |> Yojson.Safe.from_string
          |> create_payload_of_yojson
        in

        `String create_payload.url
        |> Yojson.Safe.to_string
        |> Dream.json);

    Dream.get "/v/:shortened"
      (fun request ->
        Dream.html("unshortened:" ^ Dream.param request "shortened")
      )
  ]
