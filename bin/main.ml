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

type create_payload = {
  url : string;
} [@@deriving yojson]

let l = Shorten.to_char(Shorten.encodenNum 2343)
let () = print_string l
let () = print_newline ()
let () = try
    print_int (Shorten.decode l)
    with
    | Failure msg -> print_endline (msg);
    |  _ -> print_endline "Unknown error occurred"

(* www.wikipedia.com/a/b/c => Link.lasso/v *)
let () =
  ignore(Db.bootstrap);
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
