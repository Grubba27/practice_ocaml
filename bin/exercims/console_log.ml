
type t = 
| Int of int 
| String of string 
| Bool of bool
| Float of float
| Char of char
| Bytes of bytes

let log (v: t) =
match v with
| (Bool v) -> Bool.to_string v |> print_string
| (Int v) -> print_int v
| (String v) ->  print_string v  
| (Float v) -> print_float v
| (Char v) -> print_char v
| (Bytes v) -> print_bytes v

type console = {log: t -> unit};;
let console = {log = log};;

console.log(Int 31);;
console.log(String "funcionou");;
console.log(Bool true);;
console.log(Float 1.9);;
console.log(Char 'f');;
console.log(Bytes Bytes.empty);;
