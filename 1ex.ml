(* 
Write a function last : 'a list -> 'a option that returns the last element of a list. (easy)

# last [ "a" ; "b" ; "c" ; "d" ];;
- : string option = Some "d"
# last [];;
- : 'a option = None
 *)
let rec last xs = match xs with 
| [] -> None
| x :: [] -> Some x
| x :: xs -> last xs;;