(* 
Write a function last : 'a list -> 'a option that returns the last element of a list. (easy)
 *)
let rec last xs = match xs with 
| [] -> None
| x :: [] -> Some x
| x :: xs -> last xs;;