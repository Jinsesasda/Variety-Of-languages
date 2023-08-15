open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = match e with
  | (Value value) -> (match value with 
  |(Int(int)) -> (Int(int))
  |(Bool(bool)) -> (Bool(bool))
  |(String(str)) -> (String(str))
  |(Closure(env, variable, expr)) -> (Closure(env, variable, expr)))

|(Not (value)) -> let expr = 
       eval_expr env value (*get the next value calling function one more time*) in 
      (match expr with 
      | Bool value -> Bool (not value)
      | _ -> raise (TypeError "TypeError Not"))

| ID id -> lookup env id
| Binop(op, expression1, expression2) -> 
   if op = Add then let i1 = eval_expr env  expression1 in
    let i2 = eval_expr env expression2 in
           (match i1 with
             | Int val1 -> (match i2 with
              | Int val2 -> Int (val1+val2)
              | _ -> raise (TypeError "TypeError happen"))
           | _ -> raise (TypeError "Type Error ouccred"))
   else if op = Sub then let i1 = eval_expr env  expression1 in
    let i2 = eval_expr env  expression2 in
            (match i1 with
            | Int val1 -> (match i2 with
              | Int val2 -> Int (val1-val2)
              | _ -> raise (TypeError "TypeError happen"))
            | _ -> raise (TypeError "Type TypeError happen"))
   else if op = Div then let i1 = eval_expr env expression1 in
    let i2 = eval_expr env  expression2 in 
            (match i1 with 
            | Int val1 -> (match i2 with
              | Int val2 -> (match val2 with 
              | 0 -> raise (DivByZeroError)
              | _ -> Int (val1/val2))
            | _ -> raise (TypeError "TypeError happen"))
          | _ -> raise (TypeError "TypeError happen"))           
   else if op = Mult then let i1 = eval_expr env expression1 in 
   let i2 = eval_expr env  expression2 in
             (match i1 with
             | Int val1 -> (match i2 with
              | Int val2 -> Int (val1*val2)
              | _ -> raise (TypeError "TypeError happen"))
            | _ -> raise (TypeError "TypeError happen"))
    else if op =Greater then let i1 = eval_expr env expression1 in 
    let i2 = eval_expr env  expression2 in
                (match i1 with
                | Int val1 -> (match i2 with
                 | Int val2 -> Bool (i1 > i2)
                 | _ -> raise (TypeError "TypeError happen"))
                | _ -> raise (TypeError "TypeError happen"))
   else if op = Less then let i1 = eval_expr env  expression1 in 
   let i2 = eval_expr env expression2 in 
             (match i1 with
             | Int val1 -> (match i2 with
              | Int val2 -> Bool (val1 < val2)
              | _ -> raise (TypeError "TypeError happen"))
             | _ -> raise (TypeError "TypeError happen"))
   else if op = GreaterEqual then let i1 = eval_expr env  expression1 in 
   let i2 = eval_expr env  expression2 in
                  (match i1 with 
                   |Int val1 -> (match i2 with
                   | Int val2 -> Bool (val1 >= val2)
                   | _ -> raise (TypeError "TypeError happen"))
                | _ -> raise (TypeError "TypeError happen"))
   else if op = LessEqual then let i1 = eval_expr env  expression1 in 
   let i2 = eval_expr env expression2 in 
                  (match i1 with
                  | Int val1 -> (match i2 with
                  | Int val2 -> Bool (val1 <= val2)
                  | _ -> raise (TypeError "TypeError happen"))
                | _ -> raise (TypeError "TypeError happen"))
   else if op = Concat then let i1 = eval_expr env  expression1 in 
   let i2 = eval_expr env  expression2 in
               (match i1 with
               | String val1 -> (match i2 with
               | String val2 -> String (val1^val2)
               | _ -> raise (TypeError "TypeError happen"))
              | _ -> raise (TypeError "TypeError happen"))
   else if op = Equal then let i1 = eval_expr env  expression1 
   in let i2 = eval_expr env  expression2 in
              (match i1 with 
              | Int val1 -> (match i2 with
                | Int val2 -> Bool (val1 = val2)
                | _ -> raise (TypeError "TypeError happen"))
              | Bool val1 -> (match i2 with
                | Bool val2 -> Bool (val1 = val2)
                | _ -> raise (TypeError "TypeError happen"))            
              | String val1 -> (match i2 with
                | String val2 -> Bool (val1 = val2)
                | _ -> raise (TypeError "TypeError happen"))   
              | _ -> raise (TypeError "TypeError happen")            
              )
     else if op = NotEqual then let i1 = eval_expr env  expression1 in 
     let i2 = eval_expr env  expression2 in
              (match i1 with 
              | Int val1 -> (match i2 with
               | Int val2 -> Bool (val1 <> val2)
               | _ -> raise (TypeError "TypeError happen"))
              | Bool val1 -> (match i2 with
               | Bool val2 -> Bool (val1 <> val2)
               | _ -> raise (TypeError "TypeError happen"))            
              | String val1 -> (match i2 with
                | String val2 -> Bool (val1 <> val2)
               | _ -> raise (TypeError "TypeError happen"))
              | _ -> raise (TypeError "TypeError happen")            
              )

     else if op = Or then let i1 = eval_expr env expression1 in let i2 = eval_expr env expression2 in
              (match i1 with 
              | Bool val1 -> (match i2 with
                           | Bool val2 -> Bool (val1 || val2)
                           | _ -> raise (TypeError "TypeError happen"))            
              | _ -> raise (TypeError "TypeError happen")          
              )
      else let i1 = eval_expr env expression1 in let i2 = eval_expr env expression2 in
              (match i1 with 
              | Bool val1 -> (match i2 with
                           | Bool val2 -> Bool (val1 && val2)
                           | _ -> raise (TypeError "TypeError happen"))            
              | _ -> raise (TypeError "TypeError happen")          
              )
  
| If(expression1, expression2, expression3) -> 
    let temp = eval_expr env expression1 in
    (match temp with 
    | Bool true_or_false -> (match true_or_false with
    |true -> (eval_expr env expression2)
    |false -> (eval_expr env expression3))
    | _ -> raise (TypeError "TypeError happen")) 
    
  
 | Fun (id, content ) -> Closure (env, id, content)

| Let(var, bool, expr1, expr2) -> 
  if bool then let env2 = extend_tmp env var in 
  update env2 var (eval_expr env2 expr1) ; eval_expr env2 expr2
  else let x = extend env var (eval_expr env expr1) in eval_expr x expr2

| FunctionCall(expr1, expr2) -> let closure = eval_expr env expr1 in
  (match closure with
    | Closure(var1, var2, var3) -> let expr = eval_expr env expr2 in 
    let temp = extend var1 var2 expr in 
    eval_expr temp var3
    | _ -> raise (TypeError "TpyeError happen"))
(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m =  match m with
  | Def (var, e) -> let x = (extend_tmp env var) in 
  let value = eval_expr x e in 
  update x var value; (x, Some value)
  | NoOp -> ([], None)
  |(Expr(express)) -> let final = eval_expr env express in
  (env, (Some final))
  ;;

 