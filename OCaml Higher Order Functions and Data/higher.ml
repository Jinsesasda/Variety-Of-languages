open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let count_occ lst target = 
  fold(fun num check->if check = target 
  then num + 1
   else num )0 lst
;;

let contains_elem lst e = 
  let num = (count_occ lst e) in 
      if num = 0 then
          false
      else
          true
;;

let is_present lst x = 
  let plst = [] in
  fold (fun plst check ->if check = x then plst@[1] else plst@[0]) plst lst
;;

  
let uniq_function lst element =
  fold (fun bool elm -> elm = element || bool) false lst
;;

let uniq lst = let result = [] in
  fold (fun result element -> if (uniq_function result element) then
      result else result@[element]) result lst
;;

let assoc_helper element lst = 
  let tup (x, y) = (x, y) in
  tup (element, (count_occ lst element))
;;

let assoc_list lst = let result_lst = 
[] in let temp = (uniq lst) in
  fold (fun result_lst element -> result_lst@[assoc_helper element lst]) result_lst temp
;;

let ap fns args = let result = [] in
  fold (fun result fns_element -> result@map fns_element args) result fns
;;

