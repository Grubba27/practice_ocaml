let make_list l = String.to_seq l |> List.of_seq;;
let nucleotides = "ACGT";;
let bad_str = "INVALID";;
let good_str = "ACTACT";;

let contains_nuclotides = String.contains nucleotides
let is_valid (str) =
  make_list str
  |> List.for_all contains_nuclotides  


let print_pairs (list) =
  let string_of_int_pair pair = match pair with
  | a,b -> (a)^": "^(string_of_int b)^"; " in
  List.map string_of_int_pair list 
  |> List.iter print_string;;

let count_unique_elements list = 
  let count_element e list = List.filter (fun x -> x = e) list |> List.length in
  List.sort_uniq String.compare list 
  |> List.map (fun e -> (e, count_element e list));;


let counter l =
  make_list l
  |> List.map (String.make 1)
  |> count_unique_elements

let count_nucleotides (l: string) = 
  if is_valid l then counter l |> print_pairs
  else print_endline "error";;

  let () = count_nucleotides good_str  ;;
  let () = count_nucleotides bad_str  ;;