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

(* www.wikipedia.com/a/b/c => Link.lasso/v *)
let () =
  ignore(Db.bootstrap);
  Dream.run
  @@ Dream.logger
  @@ Dream.router [

    Dream.get "/" (fun _ -> Dream.html "Good morning, world!");

    Dream.post "/shorten"
      (fun request ->
        let%lwt body = Dream.body request in
        let create_payload =
          body
          |> Yojson.Safe.from_string
          |> create_payload_of_yojson
        in
        let shortened =
          Db.insert_into create_payload.url
          |> Shorten.encode 
        in
        Dream.log "shortened %s"  shortened;
        `String shortened
        |> Yojson.Safe.to_string
        |> Dream.json);

    Dream.get "/v/:shortened"
      (fun request ->
        let req_str = Dream.param request "shortened" in
        let numeric = Shorten.decode (req_str) in
        match (Db.get_longurl numeric) with
          | Some url -> Dream.redirect request url
          | _ -> Dream.not_found request
      )
  ]
