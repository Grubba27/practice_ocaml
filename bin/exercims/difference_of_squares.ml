open List
open Printf
let (--) i j =
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j [] ;;
let range s e = (s--e);;
let r = range 1 10;;

let () = List.iter (printf "%d ") r;;
let () = print_endline " Range fn"

let square base = base * base;;

let p = square 2;;

let () = print_int p;;
let () = print_endline " square"


let list_sum = List.fold_left (+) 0;;
let ls = list_sum (1--3);;

let () = print_int ls;;
let () = print_endline " Sum"

let list_square l = List.map square l;;
let l_square = list_square (1--3);;

let () =  l_square |> list_sum |> print_int ;;
let () = print_endline " list square"

let sum_of_range =
 range 1 10 |> list_sum ;;

let () = print_int sum_of_range;;
let () = print_endline " Sum of range"

let square_of_sum n =
  range 1 n
  |> list_sum
  |> square ;;

let () = square_of_sum 10 |> print_int ;;
let () = print_endline " Square of sum"

let sum_of_squares n =
  range 1 n
  |> list_square
  |> list_sum ;;

let () = sum_of_squares 10 |> print_int ;;
let () = print_endline " Sum of squares"

let difference_of_squares n =
  (square_of_sum n) - (sum_of_squares n)

let () = difference_of_squares 10 |> print_int ;;
let () = print_endline " Result"
