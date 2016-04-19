(*
	Find out whether a list is a palindrome. (easy)

HINT: a palindrome is its own reverse.

# is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
- : bool = true
# not (is_palindrome [ "a" ; "b" ]);;
- : bool = true
*)

let rec is_palindrome xs = 
	let rec is_palindrome' ys xs = match ys, xs with
	| [] , []-> true 
	| y :: ys, x :: xs  -> if(y = x) then is_palindrome' ys xs else false
	in is_palindrome' (rev xs) xs ;;

let rec rev' ys = function
| [] -> ys
| x :: xs -> rev' (x :: ys)  xs;;

let rev xs =  rev' [] xs;; 	

let is_palindrome list =
    list = List.rev list;;