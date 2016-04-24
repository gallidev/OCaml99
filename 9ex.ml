(* Pack consecutive duplicates of list elements into sublists. (medium)

Solution

# pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
- : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]]
  *)

let pack l = 
	let rec aux list = function 
	| [] -> [list]
	| a :: b :: [] -> if(a = b) then aux (a::b::list) [] else (a::list) :: (aux [b] [])
	| a :: b :: xs -> if(a = b) then aux (a::list) (b::xs) else (a::list) :: (aux [] (b::xs))
in aux [] l;;