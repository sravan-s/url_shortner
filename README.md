# url_shortner
Url shortner with ocaml

I am not looking to make large scale thing from system design interviews

This is to learn ocaml ergonomic

Probably, a server in SQLITE + small in memory cache will do :)

(*
  https://stackoverflow.com/questions/742013/how-do-i-create-a-url-shortener
   (1) Save URL in database
   (2) Get unique row ID for that URL from database
   (3) Convert integer ID to short string with encode(), e.g. 273984 to f5a4
   (4) Use the short string (e.g. f4a4) in your sharable URLs
   (5) When receiving a request for a short string (e.g. 20a8), decode the string to an integer ID with decode()
   (6) Look up URL in database for given ID
*)

To do: LFU caching at `Dream.get "/v/:shortened"` to reduce DB usage
