(*
Find the number of elements of a list. (easy)
	OCaml standard library has List.length but we ask that you reimplement it. Bonus for a tail recursive solution.
	# length [ "a" ; "b" ; "c"];;
- : int = 3
# length [];;
- : int = 0
*)

let rec length' n = function
| [] -> n
| x :: xs -> length' (n+1) xs;;

let length xs = 
	let n = 0 in 
	length' n xs;;


