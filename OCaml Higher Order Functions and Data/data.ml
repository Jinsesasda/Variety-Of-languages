open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode (x, None, IntLeaf, IntLeaf, IntLeaf)
  | IntNode (left, right, lLeaf, mLeaf, rLeaf) ->

    if x = left || Some x = right 
    then t

    else if right = None && x < left 
    then IntNode (x, Some left, lLeaf, mLeaf, rLeaf)

    else if right = None && x > left 
    then IntNode (left, Some x, lLeaf, mLeaf, rLeaf)

    else if Some x > right 
    then IntNode (left, right, lLeaf, mLeaf, (int_insert x rLeaf))
    
    else if x < left then IntNode (left, right, (int_insert x lLeaf), mLeaf, rLeaf)
    else IntNode (left, right, lLeaf, (int_insert x mLeaf), rLeaf)
;;

let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (left, right, lLeaf, mLeaf, rLeaf) ->
      if x = left || Some x = right then true
      else if Some x > right then int_mem x rLeaf
      else if x < left then int_mem x lLeaf
      else int_mem x mLeaf
;;

let rec int_size t =let size = 0 in
  match t with
  | IntLeaf -> size
  | IntNode (left, right, lLeaf, mLeaf, rLeaf) ->
    if left >= 0 && right >= Some 0 then 
      size + 2 + int_size lLeaf + int_size mLeaf + int_size rLeaf
    else 
      size + 1
;;

let convert_int x = match x with
  | None -> 0
  | Some a -> a
;;

let get_max t = match t with
  | IntLeaf -> 0
  | IntNode (left, right, lLeaf, mLeaf, rLeaf) -> convert_int right
;;



let rec int_max t =
  match t with
  | IntLeaf -> raise (Invalid_argument("int_max"))
  | IntNode (left, right, lLeaf, mLeaf, rLeaf) -> let max = convert_int right in
    if get_max rLeaf > max then 
      int_max rLeaf
    else 
      max
;;
