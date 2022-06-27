(*type nucleotide = A | C | G | T*)

let make_list l = String.to_seq l
  |> List.of_seq
  |> List.map (String.make 1);;

let eq_counter acc l1 l2  =
  if String.compare l1 l2 == 0
    then acc
    else acc ^ "1";;

let err_count (l1: string) (l2) =
  make_list l1
  |> List.fold_left2 eq_counter "" l2;;

let diff l1 l2 =
  make_list l2
  |> err_count l1 ;;

let hamming_distance l1 l2 =
  diff l1 l2
    |> make_list
    |> List.length;;

let list1 = "A"
let list2 = "B"

let () = hamming_distance list1 list2 |> print_int
