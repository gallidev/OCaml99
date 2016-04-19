(*Eliminate consecutive duplicates of list elements. (medium)

# compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string list = ["a"; "b"; "c"; "a"; "d"; "e"]*)

let compress list =
	let rec aux last list final = match list with
	| [] -> List.rev(final)
	| x :: [] -> if(x = last) then List.rev(final) else aux x [] (x :: final)
	| x :: xs -> if(x = last) then aux last xs final else aux x xs (x ::final)
in aux  (List.hd list) (List.tl list) ([List.hd list]);; 