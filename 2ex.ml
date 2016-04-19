(*
	Find the last but one (last and penultimate) elements of a list. (easy)

	# last_two [ "a" ; "b" ; "c" ; "d" ];;
- : (string * string) option = Some ("c", "d")
# last_two [ "a" ];;
- : (string * string) option = None
*)

let rec last_two xs = match xs with 
| [] -> None
| a :: b ::  [] -> Some (a,b)
| _ :: xs -> last_two xs;;