(* hashtable of items -> store *)
(* frequency bucket *)
(* node-list of items with same frequncy *)
type 'a node = {
  mutable value: 'a;
  mutable next: 'a node option;
  mutable prev: 'a node option;
}

type key_list_node = string node

type freq_bucket_node = {
  mutable value: int;
  mutable child_from: key_list_node node option; 
  mutable child_to: key_list_node node option; 
}

type lfu_store = (string, string) Hashtbl.t

type lfu = {
  mutable store: lfu_store;
  mutable frequency_bucket: freq_bucket_node;
}

let create (size: int): lfu =
  let store = Hashtbl.create size in
  let bucket: freq_bucket_node = { value = 0; child_from = None; child_to = None; } in
  {
    store = store;
    frequency_bucket = bucket; 
  }

let get (key: string): string option =
  Some (key ^ "todo")

let put (key: string) (value: string): (bool, bool) result =
  match key ^ value with
  | "todo" -> Ok(true)
  | _ -> Error(false)

