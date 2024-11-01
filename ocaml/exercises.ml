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

open Printf
open Sys

let meaningful_line_count filename =
  let ic = open_in filename in
  let rec count_lines acc =
    try
      let line = input_line ic in
      let trimmed_line = String.trim line in
      if trimmed_line <> "" && not (String.starts_with ~prefix:"#" trimmed_line) then
        count_lines (acc + 1)
      else
        count_lines acc
    with End_of_file ->
      close_in ic;
      acc
  in
  count_lines 0

(* Write your shape type and associated functions here *)

(* Write your binary search tree implementation here *)
