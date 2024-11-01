exception Negative_Amount

let change amount =
  if amount < 0 then
    raise Negative_Amount
  else
    let denominations = [25; 10; 5; 1] in
    let rec aux remaining denominations =
      match denominations with
      | [] -> []
      | d :: ds -> (remaining / d) :: aux (remaining mod d) ds
    in
    aux amount denominations

let first_then_apply lst predicate transform =
  try
    let first = List.find predicate lst in
    transform first
  with Not_found -> None

let powers_generator base =
  let rec aux n () =
    Seq.Cons (n, aux (n * base))
  in
  aux 1

(* Write your line count function here *)

(* Write your shape type and associated functions here *)

(* Write your binary search tree implementation here *)
