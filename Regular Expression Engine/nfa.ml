open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma : 's list;
  qs : 'q list;
  q0 : 'q;
  fs : 'q list;
  delta : ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s : string) : char list =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
 


(****************)
(* Part 1: NFAs *)
(****************)


let move (nfa : ('q, 's) nfa_t) (qs : 'q list) (s : 's option) : 'q list =
  (*check move*)
  let lst1 =[] in 
  let lst2 =[] in
  (*if the symbol is not in the alphabet sigma, then return an empty list*)
  List.fold_left(fun a (state: 'q) ->
   a @ (List.fold_left (fun cul (t: ('q, 's) transition) -> 
  let ( state1, state2, state3 (*making the tuple*)) = t in

    if(state2 = s && state = state1 ) then
      if Sets.elem state3 a then cul
      else cul @ [state3]
    else cul
   )lst2 nfa.delta) 
  ) lst1 qs
;;

  let rec helper n v q = match n with {sigma; qs; q0; fs; delta} ->
   if not(v = q)
   then helper n q (List.fold_left (fun cul t -> 
      match t with (state1, t, state2) ->
      if t =  None && (Sets.elem state2 cul) = false && (Sets.elem state1 q) = true 
        then [state2] @ cul 
      else cul) q delta)
  else q

let e_closure(nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  List.fold_left(fun cul list -> if (Sets.elem list cul) = false
   then 
  list::cul  (*Since this is list need to use ::*)
  else cul) [] (helper nfa [] qs)

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa : ('q, 's) nfa_t) (qs : 'q list) : 'q list list =
  let lst = [] in List.fold_left(fun a (s: 's) ->
   a @ [(e_closure nfa (move nfa qs (Some s)))])lst nfa.sigma 



let new_trans (nfa : ('q, 's) nfa_t) (qs : 'q list) :
    ('q list, 's) transition list =
    let lst = [] in List.fold_left(fun a (s: 's) -> 
    a @ [(qs, Some s,e_closure nfa (move nfa qs (Some s)))]) lst nfa.sigma 



let new_finals (nfa : ('q, 's) nfa_t) (qs : 'q list) : 'q list list =
  let lst = [] in List.fold_left(fun a (state: 'q) ->
   if Sets.elem state nfa.fs 
   then [qs] else [])
   lst qs

let rec nfa_to_dfa_step (n : ('q, 's) nfa_t) (d : ('q list, 's) nfa_t)
    (work : 'q list list) : ('q list, 's) nfa_t =
    match work with
    (*if work is empty, it means it is dfa*)
    |[] -> d 
    (*if work is not empty, it will go through the steps to check*)
    | a::b -> let n_to_d = {
          q0 = d.q0;
          qs = (Sets.insert a d.qs); 
          sigma = d.sigma; 
          fs = (Sets.union d.fs (new_finals n a)); 
          delta = (Sets.union d.delta (new_trans n a))} 
          
          in let k = Sets.diff (Sets.union [] (new_states n a)) d.qs in
          (*Using this Sets.diff, check the dfa and nfa*) 
          nfa_to_dfa_step n n_to_d (Sets.union b k) 
          
          let nfa_to_dfa (n: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
            nfa_to_dfa_step n {
              
              
              sigma = n.sigma; 
              q0 = e_closure n [n.q0];

              qs = [e_closure n [n.q0]];

              (*let the fs first empty*)
              fs = []; 
              (*let the delta first empty*)
              delta = []
             } 
             (*return value *)
             [e_closure n [n.q0]];; 
          
           (*checking the type of the accept using helper function*)  
          let rec checking_acc (n: ('q, char) nfa_t) (m: 'q list)  (char: char list) : bool =
            match char with
              | [] -> let e_closure_check = e_closure n m in 
              (List.fold_left (fun cul value -> (Sets.subset [value] n.fs) || cul) false e_closure_check) && e_closure_check != []
              | a::b -> checking_acc n (move n (e_closure n m) (Some a)) b
          
          let accept (n: ('q, char) nfa_t) (m: string) : bool =
           (*checking m is empty or not*) 
           if m != "" 
           then let (char: 's list) = explode m in 
           checking_acc n [n.q0] char
            else subset (e_closure n [n.q0]) n.fs
            ;; 
