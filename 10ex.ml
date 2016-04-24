(* Run-length encoding of a list. (easy)

If you need so, refresh your memory about run-length encoding.

Solution

Here is an example:

# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] *)

let encode list = 
	let rec separe list acc times = match list with
	| [] -> List.rev acc
	| a :: [] -> separe [] (((times + 1), a) :: acc) 0
	| a :: b :: xs -> if(a = b) then separe (b::xs) acc (times + 1) 
						else separe (b::xs) (((times + 1), a) :: acc) 0
	in separe list [] 0;;
