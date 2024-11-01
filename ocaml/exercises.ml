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

type shape =
  | Sphere of float
  | Box of float * float * float

let volume s =
  match s with
  | Sphere r -> (4.0 /. 3.0) *. Float.pi *. r ** 3.0
  | Box (w, l, d) -> w *. l *. d

let surface_area s =
  match s with
  | Sphere r -> 4.0 *. Float.pi *. r ** 2.0
  | Box (w, l, d) -> 2.0 *. (w *. l +. w *. d +. l *. d)

type 'a bst =
  | Empty
  | Node of 'a * 'a bst * 'a bst

let rec size tree =
  match tree with
  | Empty -> 0
  | Node (_, left, right) -> 1 + size left + size right

let rec contains x tree =
  match tree with
  | Empty -> false
  | Node (v, left, right) ->
      if x = v then true
      else if x < v then contains x left
      else contains x right

let rec insert x tree =
  match tree with
  | Empty -> Node (x, Empty, Empty)
  | Node (v, left, right) ->
      if x = v then tree
      else if x < v then Node (v, insert x left, right)
      else Node (v, left, insert x right)

let rec inorder tree =
  match tree with
  | Empty -> []
  | Node (v, left, right) -> inorder left @ [v] @ inorder right

let rec to_string tree =
  match tree with
  | Empty -> "()"
  | Node (v, left, right) ->
      let left_str = to_string left in
      let right_str = to_string right in
      if left_str = "()" && right_str = "()" then
        Printf.sprintf "(%s)" (string_of_int v)
      else
        Printf.sprintf "(%s%s%s)" left_str (string_of_int v) right_str
