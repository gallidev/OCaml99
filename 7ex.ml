(*
	Flatten a nested list structure. (medium)

	# (* There is no nested list type in OCaml, so we need to define one
     first. A node of a nested list is either an element, or a list of
     nodes. *)
  type 'a node =
    | One of 'a 
    | Many of 'a node list;;
type 'a node = One of 'a | Many of 'a node list

# flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
- : string list = ["a"; "b"; "c"; "d"; "e"]
*)

  type 'a node =
    | One of 'a 
    | Many of 'a node list;;

let rec flatten = function 
  | One x :: [] -> [x] 
  | One x :: xs -> x :: (flatten xs) 
  | Many x :: [] -> flatten x 
  | Many x :: xs -> (flatten x) @ flatten xs ;;

  (*their solution*)
   let flatten list =
    let rec aux acc = function
      | [] -> acc
      | One x :: t -> aux (x :: acc) t
      | Many l :: t -> aux (aux acc l) t in
    List.rev (aux [] list);;