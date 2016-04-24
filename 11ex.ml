(* Modified run-length encoding. (easy)

Modify the result of the previous problem in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

Since OCaml lists are homogeneous, one needs to define a type to hold both single elements and sub-lists.

# type 'a rle =
    | One of 'a
    | Many of int * 'a;;
type 'a rle = One of 'a | Many of int * 'a
Solution

# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")] *)

let encode list = 
	let rec separe list acc times = match list with
	| [] -> List.rev acc
	| a :: [] -> if(times = 0) then separe [] ((One a) :: acc) 0 
							else separe [] ((Many (times+1,a)) :: acc) 0 
	| a :: b :: xs -> if(a = b) then separe (b::xs) acc (times + 1) 
						else if(times = 0) then separe (b::xs) ((One a) :: acc) 0 
							else separe (b::xs) ((Many (times+1,a)) :: acc) 0 

	in separe list [] 0;;
