(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = match tup with
|(a,b,c) -> (c,b,a)
;;
let is_odd x = 
  if x mod 2 = 0 then
   false 
   else 
    true
;;
let area x y = match x,y with
 |(a,b),(c,d) ->  abs((c-a)*(d-b))
;;

let volume x y = match x,y with
|(a,b,c),(d,e,f) -> abs((d-a) * (e-b) * (f-c)) 
;;





(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = 
  if n = 0 then 0
 else if n < 3 then 1
else fibonacci (n - 1) + fibonacci (n - 2)
;;

let rec pow x y = 
  if y = 0 then 1
    else x * pow x (y - 1)
;;

let rec log x y = 
  if x > y then 0
    else 1 + log x (y/x)
    ;;


let rec gcf x y = 
  if y = 0 then x
    else gcf y (x mod y)
;;

let rec is_prime x = 
  if x < 2 then false
else let n = x - 1 in
  let rec primeOrNot x n =
      if n = 1 then true
      else if (x mod n) = 0 then false
      else primeOrNot x (n - 1) in primeOrNot x n
;;

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = 
  match (idx, lst) with
    | _, [] -> failwith "Out of bounds" 
    | (idx, _) when idx < 0 -> failwith "Out of bounds"
    | 0, h::_ -> h
    | idx, h::t -> get (idx - 1) t
;;
let rec helper lst = 
  match lst with
  | [] -> 0
  | _::t -> 1 + helper t
;;

let larger lst1 lst2 = 
  if helper lst1 < helper lst2 then lst2
    else if helper lst1 > helper lst2 then lst1
    else []
;;

let reverse lst =
  let rec helper_function lst1 lst2 = 
    match lst2 with

    |[] -> lst1

    | h::t -> helper_function (h::lst1) t in helper_function [] lst
;;

let rec combine lst1 lst2 = 
  match (lst1, lst2) with
    |([], []) -> []
    |(_, []) -> lst1
    |([], _) -> lst2
    |(h1::t1, h2::t2) -> h1::(combine t1 lst2)
;;

let rec merge lst1 lst2 = 
  match (lst1, lst2) with
    |([], []) -> []
    |(_, []) -> lst1
    |([], _) -> lst2
    |(h1::t1, h2::t2) -> if h1 < h2 
    then h1::merge t1 lst2
    else h2::merge lst1 t2
;;

let rec rotate shift lst = 
  match (shift, lst) with
    | (0, _) -> lst
    | (_, []) -> []
    | (_, h::t) -> rotate (shift - 1) (t@(h::[]))
;;

let rec is_palindrome lst = 
  lst = reverse lst
;;