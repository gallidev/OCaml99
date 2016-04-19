(*
	Reverse a list. (easy)

OCaml standard library has List.rev but we ask that you reimplement it.
# rev ["a" ; "b" ; "c"];;
- : string list = ["c"; "b"; "a"]
*)

let rec rev' ys = function
| [] -> ys
| x :: xs -> rev' (x :: ys)  xs;;

let rev xs =  rev' [] xs;;