
let splitter (s: string) = String.split_on_char " ".[0] s;;
let png = "Pfsadf Njhdasgfs Gfasdf"
let get_str_at str index = String.make 1 str.[index] |> String.capitalize_ascii 
let r (str: string) acc k = 
  match ((String.contains str '_'), (String.contains str '-')) with 
  | (false, false) -> str ^ acc 
  | (_,_) -> get_str_at k 1 ^ acc
let acronym_maker acc k =
  get_str_at k 0 
  |> r acc k
  
let acronym (str: string) =
   splitter str 
   |> List.fold_left acronym_maker "";;

let () = acronym png |> print_endline 