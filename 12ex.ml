(* Decode a run-length encoded list. (medium)

Given a run-length code list generated as specified in the previous problem, construct its uncompressed version.

Solution

# decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
- : string list =
["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] *)

let decode list =
	let rec add times letter acc = match times with 
	| 0 -> acc
	| x -> add (x-1) letter (letter :: acc)
	in    
	let rec help list acc = match list with
	| [] -> List.rev acc
	| x :: xs -> match x with 
				| Many (a,b) -> help xs (add a b acc) 
				| One a -> help xs (add a 1 acc)
	in
	help list [];;