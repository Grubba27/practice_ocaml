
let pling (v: int) =
  if v mod 3 == 0 then "Pling" else "";;
let plang (v: int) =
  if v mod 5 == 0 then "Plang" else "";;
let plong (v: int) =
  if v mod 7 == 0 then "Plong" else "";;

let raindrop2 (v: int) =
  let word v = (pling v) ^( plang v) ^ (plong v) in
  if word v == "" then string_of_int v else word v ;;

let raindrop (v: int) =
  match ((v mod 3), (v mod 5), (v mod 7)) with
    | (0,0,0) -> "PlingPlangPlong"
    | (0,0,_) -> "PlingPlang"
    | (0,_,0) -> "PlingPlong"
    | (_,0,0) -> "PlangPlong"
    | (0,_,_) -> "Pling"
    | (_,0,_) -> "Plang"
    | (_,_,0) -> "Plong"
    | (_,_,_) -> string_of_int v ;;


let _ = raindrop2 30 |> print_endline