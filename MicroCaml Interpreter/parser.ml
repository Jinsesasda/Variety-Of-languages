open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks =
   match lookahead toks with
    |Some Tok_Let -> parse_let toks
    |Some Tok_Fun -> parse_fun toks
    |Some Tok_If -> parse_if toks
    |_ -> parse_or toks

and parse_let toks = 
let tempToken = match_token toks Tok_Let in
  (*get the left token*)
  let (t, expression) = 
  let tok = lookahead tempToken in
  if tok = Some Tok_Rec then ((match_token tempToken (*get the left token after rec*) Tok_Rec), true)
  else (tempToken, false) in (*if there is rec after let token, the value of expression will be true else it will be false*)
  let token = t in 
          
  let id = match token with 
  | [] -> raise (InvalidInputException "Token is not here")
  | a::b -> match a with
   | Tok_ID id -> id
   | _ -> raise (InvalidInputException "error happen") in
     let token = match_token token (Tok_ID (match token with
     | [] -> raise (InvalidInputException "Token is not here")
     | a::b-> match a with
     | Tok_ID id -> id          
     | _ -> raise (InvalidInputException "error happen"))) in 
      let token = match_token token Tok_Equal in 
      let (t1, e1) = parse_expr token in 
      let token = t1 in 
      let token = match_token token Tok_In in 
      let (t2, e2) = parse_expr token in let token = t2 in token, Let (id, expression, e1, e2)



and parse_if toks =
  let tempToken = match_token toks Tok_If in
    let (t1, e1) = parse_expr tempToken in
     let  tempToken1 = match_token t1 Tok_Then in
    let (t2, e2) = parse_expr tempToken1 in 
    let tempToken2 = match_token t2 Tok_Else in
    let (t3, e3) = parse_expr tempToken2 in 
    t3, If (e1, e2, e3)

and parse_fun toks = 
let token = match_token toks Tok_Fun in 
  let id = match token with
    | [] -> raise (InvalidInputException "There is no token")
    | head::tail -> match head with
      | Tok_ID s -> s
      | Tok_String s -> s
      | _ -> raise (InvalidInputException "no match") in 
      let token = match_many token [(Tok_ID (match token with
        | [] -> raise (InvalidInputException "There is no token")
        | head::tail -> match head with
          | Tok_ID s -> s
          | Tok_String s -> s
          | _ -> raise (InvalidInputException "no match"))); Tok_Arrow;] in 
          let (t1, e1) = parse_expr token in t1, Fun (id, e1)


and parse_or toks = 
 let (t1, expr1) = parse_and toks in
     let tempTok = lookahead t1 in
      if tempTok = Some Tok_Or then
    let token = match_token t1 Tok_Or in let (t2, expr2) = parse_or token in
     (t2, Binop (Or, expr1, expr2))
    else
     (t1, expr1)
     
and parse_and toks = 
let (t1, expr1) = parse_equals toks in
    let tempTok = lookahead t1 in     
      if tempTok = Some Tok_And then
    let token = match_token t1 Tok_And in
   let (t2, expr2) = parse_and token in (t2, Binop (And, expr1, expr2))
    else (t1, expr1)
      
and parse_equals toks = 
let (t1, expr1) = parse_rel toks in
   let tempToken = lookahead t1 in
     if tempToken = Some Tok_Equal then
      let token = match_token t1 Tok_Equal in 
      let (t2, expr2) = parse_equals token in (t2, Binop (Equal, expr1, expr2))      
     else if tempToken = Some Tok_NotEqual then
      let token = match_token t1 Tok_NotEqual in 
      let (t2, expr2) = parse_equals token in (t2, Binop (NotEqual, expr1, expr2))
      else (t1, expr1)

and parse_rel toks = 
let (t1, expr1) = parse_add toks in
    let tempToken = lookahead t1 in 
       if tempToken = Some Tok_Less then
       let real = match_token t1 Tok_Less 
       in let (t2, expr2) = parse_rel real in 
       (t2, Binop (Less, expr1, expr2))

         else if tempToken = Some Tok_GreaterEqual then
         let real = match_token t1 Tok_GreaterEqual 
         in let (t2, expr2) = parse_rel real in
          (t2, Binop (GreaterEqual, expr1, expr2))  

       else if tempToken = Some Tok_Less then
        let real = match_token t1 Tok_Less 
        in let (t2, expr2) = parse_rel real in 
        (t2, Binop (Less, expr1, expr2))

        else if tempToken = Some Tok_Greater then
        let real = match_token t1 Tok_Greater in let (t2, expr2) = parse_rel real in
         (t2, Binop (Greater, expr1, expr2))

       
       else if tempToken = Some Tok_LessEqual then
      let real = match_token t1 Tok_LessEqual in 
      let (t2, expr2) = parse_rel real in 
      (t2, Binop (LessEqual, expr1, expr2))

      else  (t1, expr1)

and parse_add toks = let (t1, expr1) = parse_multiply toks in
    let tempToken = lookahead t1 in
      if tempToken = Some Tok_Sub then
       let real1 = match_token t1 Tok_Sub in let (t2, expr2) = parse_add real1 in 
       (t2, Binop (Sub, expr1, expr2))
      else if tempToken = Some Tok_Add then
       let real = match_token t1 Tok_Add in let (t2, expr2) = parse_add real in 
       (t2, Binop (Add, expr1, expr2))
      
      else (t1, expr1)

and parse_multiply toks = let (t1, expr1) = parse_concat toks in
    let tempToken = lookahead t1 in
         if tempToken = Some Tok_Mult then 
         let token = match_token t1 Tok_Mult in 
         let (t2, expr2) = parse_multiply token in 
         (t2, Binop (Mult, expr1, expr2))

      else if tempToken = Some Tok_Div then
       let token = match_token t1 Tok_Div in 
       let (t2, expr2) = parse_multiply token in 
       (t2, Binop (Div, expr1, expr2))

      else (t1, expr1)

and parse_concat toks = let (t1, expr1) = parse_unary toks in
    let tempToken = lookahead t1 in
      if tempToken = Some Tok_Concat then
       let token = match_token t1 Tok_Concat in 
       (*need to repeat the parse_concat function until it get the value*)
       let (t2, expr2) = parse_concat token in 
       (t2, Binop (Concat, expr1, expr2))
      else (t1, expr1)

and parse_unary toks = 
  let tempToken = lookahead toks in
  if tempToken = Some Tok_Not then 
  let token = match_token toks Tok_Not in 
  let (t1, expr1) = parse_unary token in (t1, Not (expr1))
  else parse_func toks

  and parse_primary toks = 
match lookahead toks with
| Some Tok_LParen -> let token = match_token toks Tok_LParen in 
let (t1, expr1) = parse_expr token in 
let token = match_token t1 Tok_RParen in (token, expr1)
| Some (Tok_Int int) -> let expr = Value (Int (match toks with
    | [] -> raise (InvalidInputException "There is no token")
    | a::b -> ((match a with
      | Tok_Int id -> id
      | _ -> raise (InvalidInputException "There is no token"))))) in let token = match_token toks (Tok_Int int) in (token, expr)            
| Some (Tok_Bool bool) -> let expr = Value (Bool (match toks with
    | [] -> raise (InvalidInputException "There is no token")
    | a::b -> ((match a with
      | Tok_Bool t -> t
      | _ -> raise (InvalidInputException "no match"))))) in let token = match_token toks (Tok_Bool bool) in (token, expr)
| Some (Tok_ID id) -> let expr = ID (match toks with
    | [] -> raise (InvalidInputException "There is no token")
    | a::b -> (match a with
      | Tok_String id -> id           
      | Tok_ID id -> id
      | _ -> raise (InvalidInputException "no match"))) in
       let token = match_token toks (Tok_ID id) in (token, expr)
| Some (Tok_String string) -> let expr = Value (String (match toks with
    | [] -> raise (InvalidInputException "There is no token")
    |a::b -> ((match a with
      | Tok_ID id -> id
      | Tok_String id -> id
      | _ -> raise (InvalidInputException "no match"))))) in let token = match_token toks (Tok_String string) in (token, expr)
| _ -> raise (InvalidInputException "no match")


and parse_func toks = let (t1, expr1) = parse_primary toks in 
      (*I can not use if in this case because I am getting two values. So I need to use match ... with*)
      match lookahead t1 with
      | Some (Tok_LParen) -> let (t2, expr2) = parse_primary t1 in
       (t2, FunctionCall (expr1, expr2))
      | Some (Tok_Int int) -> let (t2, expr2) = parse_primary t1 in 
      (t2, FunctionCall (expr1, expr2))
      | Some (Tok_Bool bool) -> let (t2, expr2) = parse_primary t1 in 
      (t2, FunctionCall (expr1, expr2))
      | Some (Tok_ID x) -> let (t2, expr2) = parse_primary t1 in 
      (t2, FunctionCall (expr1, expr2))
      | Some (Tok_String string) -> let (t2, expr2) = parse_primary t1 in 
      (t2, FunctionCall (expr1, expr2))
      | _ -> (t1, expr1)




(* Part 3: Parsing mutop *)

let rec parse_mutop toks = match lookahead toks with
   
 | Some Tok_Def -> let token = match_token toks Tok_Def in
      let id = 
    ((match token with
 | [] -> raise (InvalidInputException "there is no token")
 | a::b -> match a with
 | Tok_ID id -> id
 | _ -> raise (InvalidInputException "doesnt match"))) in
  let token = match_many token [Tok_ID (id); Tok_Equal;] in 
  let (t1, expr1) = parse_expr token in
  let token = match_token t1 Tok_DoubleSemi in token, Def (id, expr1)
  | Some Tok_DoubleSemi -> let token = match_token toks Tok_DoubleSemi in (token, NoOp)       
  | _ -> let (t1, expr1) = parse_expr toks in 
  let token = match_token t1 Tok_DoubleSemi in (token, Expr (expr1))