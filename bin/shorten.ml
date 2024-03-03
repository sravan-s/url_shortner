(* We keep things simple, no upper case, no nums *)
let seed = 
  ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 
   'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']
let seed_len = List.length seed

(* Encode a number to List[base<seed_len>], in this case base10 -> base26 *)
(* 352 -> [13; 14] *)
(* my attempt at recursion *)
let rec encodenNum ?(accum: int list = []) n =
  let quotient = n / seed_len in
  let modulo = n mod seed_len in
  let new_list = (modulo::accum) in
    if quotient > 0 then
      encodenNum ~accum:new_list quotient
    else
      new_list

(* Converts ['M'; 'N'] to "MN" *)
let cl2s cl = String.concat "" (List.map (String.make 1) cl)

(* converts list from encodenNum to string *)
(* [13; 14] -> "MN" *)
let to_char li = cl2s @@ List.map (fun item -> (List.nth seed item)) li

(* saw this recursion approach in tutorials *)
let idx_in_seed (n: char) =
  let rec find_index_helper lst x index =
    match lst with
    | [] -> failwith "Invalid character"
    | hd :: t1 ->
      if hd = x then
        index
      else
        find_index_helper t1 x (index + 1)
  in find_index_helper seed n 0


(* [1, 33, 7] = `(1 * 24^2) + (33*24^1) + (7*24^0) *)
let int_list_to_num (li: int list): int =
  let folder_fn (i: int) ((total, pos): (int * int)): (int * int) =
    ((total + (i * int_of_float(float_of_int(seed_len) ** float_of_int(pos)))), (pos + 1)) in
  let (total, _) = List.fold_right folder_fn li (0, 0) in
  total

(* "MN" to ['M'; 'N'] *)
let string_to_char_list str =
  let char_seq = String.to_seq str in
  List.of_seq char_seq

(* "MN" to 352 *)
let decode (li: string) =
  string_to_char_list li
  |> List.map idx_in_seed
  |> int_list_to_num

let encode l = to_char @@ encodenNum l

(* Examples *)
(* let _ = insert_into("asdassdas") *)
(* let l = to_char(encodenNum 2343)
let () = print_string l
let () = print_newline ()
let () = try
    print_int (decode l)
    with
    | Failure msg -> print_endline (msg);
    |  _ -> print_endline "Unknown error occurred" *)
