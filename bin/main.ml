open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type message_object = {
  message : string;
} [@@deriving yojson]

(* www.wikipedia.com/a/b/c => Link.lasso/v *)

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [

    Dream.get "/"
      (fun _ ->
        Dream.html "Good morning, world!");

    Dream.post "/"
      (fun request ->
        let%lwt body = Dream.body request in

        let message_object =
          body
          |> Yojson.Safe.from_string
          |> message_object_of_yojson
        in

        `String message_object.message
        |> Yojson.Safe.to_string
        |> Dream.json);

    Dream.get "/echo/:word"
      (fun request ->
        Dream.html ((Dream.param request "word") ^ " , hola"));

  ]

