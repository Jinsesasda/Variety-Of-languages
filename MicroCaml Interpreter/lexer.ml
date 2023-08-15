open TokenTypes

(* PASTE YOUR LEXER FROM P4A HERE *)

let tokenize input = let re_lparen = Str.regexp "(" in
let re_rparen = Str.regexp ")" in
let re_equal = Str.regexp "=" in 
let re_notEqual = Str.regexp "<>" in
let re_greater = Str.regexp ">" in
let re_less = Str.regexp "<" in
let re_greaterEqual = Str.regexp ">=" in
let re_lessEqual = Str.regexp "<=" in
let re_or = Str.regexp "||" in
let re_and = Str.regexp "&&" in
let re_not = Str.regexp "not" in
let re_if = Str.regexp "if" in
let re_then = Str.regexp "then" in
let re_else = Str.regexp "else" in
let re_add = Str.regexp "+" in
let re_sub = Str.regexp "-" in
let re_mult = Str.regexp "*" in
let re_div = Str.regexp "/" in
let re_concat = Str.regexp "\\^" in
let re_let = Str.regexp "let" in
let re_def = Str.regexp "def" in
let re_in = Str.regexp "in" in
let re_rec = Str.regexp "rec" in
let re_fun = Str.regexp "fun" in
let re_arrow = Str.regexp "->" in
let re_doubleSemi = Str.regexp ";;" in

(* tokens with complex rules *)
let re_bool = Str.regexp "true\\|false" in 
let re_positiveInt = Str.regexp "[0-9]+" in
let re_negativeInt = Str.regexp "(-[0-9]+" in
let re_string = Str.regexp "\"[^\"]*\"" in
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in
let re_space = Str.regexp "[ \t\n\r]*" in

let rec tok s pos =
  if pos >= String.length s then []

  else if (Str.string_match re_positiveInt s pos)
    then let token = Str.matched_string s in 
    (Tok_Int (int_of_string token))::(tok s (Str.match_end()))

    else if(Str.string_match re_negativeInt s pos)
    then let token = Str.matched_string s in 
    (Tok_Int (int_of_string (String.sub token 1 ((String.length token)-1))))::(tok s (Str.match_end()+1))
    else if(Str.string_match re_lparen s pos) then (Tok_LParen)::(tok s (pos+1))
    else if(Str.string_match re_rparen s pos) then (Tok_RParen)::(tok s (pos+1))
    else if(Str.string_match re_equal s pos) then (Tok_Equal)::(tok s (pos+1))
    else if(Str.string_match re_notEqual s pos) then (Tok_NotEqual)::(tok s (pos+2))
    else if(Str.string_match re_greater s pos) then (Tok_Greater)::(tok s (pos+1))
    else if(Str.string_match re_less s pos) then (Tok_Less)::(tok s (pos+1))
    else if(Str.string_match re_greaterEqual s pos) then (Tok_GreaterEqual)::(tok s (pos+2))
    else if(Str.string_match re_lessEqual s pos) then (Tok_LessEqual)::(tok s (pos+2))
    else if(Str.string_match re_or s pos) then (Tok_Or)::(tok s (pos+2))
    else if(Str.string_match re_and s pos) then (Tok_And)::(tok s (pos+2))
    else if(Str.string_match re_not s pos) then (Tok_Not)::(tok s (pos+3))
    else if(Str.string_match re_if s pos) then (Tok_If)::(tok s (pos+2))
    else if(Str.string_match re_arrow s pos) then (Tok_Arrow)::(tok s (pos+2))
    else if(Str.string_match re_then s pos) then (Tok_Then)::(tok s (pos+4))
    else if(Str.string_match re_else s pos) then (Tok_Else)::(tok s (pos+4))
    else if(Str.string_match re_add s pos) then (Tok_Add)::(tok s (pos+1))
    else if(Str.string_match re_sub s pos) then (Tok_Sub)::(tok s (pos+1))
    else if(Str.string_match re_mult s pos) then (Tok_Mult)::(tok s (pos+1))
    else if(Str.string_match re_div s pos) then (Tok_Div)::(tok s (pos+1))
    else if(Str.string_match re_concat s pos) then (Tok_Concat)::(tok s (pos+1))

    else if(Str.string_match re_let s pos) then (Tok_Let)::(tok s (pos+3))
    else if(Str.string_match re_def s pos) then (Tok_Def)::(tok s (pos+3))
    else if(Str.string_match re_in s pos) then (Tok_In)::(tok s (pos+2))
    else if(Str.string_match re_rec s pos) then (Tok_Rec)::(tok s (pos+3))
    else if(Str.string_match re_fun s pos) then (Tok_Fun)::(tok s (pos+3))      
    else if(Str.string_match re_doubleSemi s pos) then (Tok_DoubleSemi)::(tok s (pos+2))
    
    else if(Str.string_match re_bool s pos) then 
    let token = Str.matched_string s in (Tok_Bool (bool_of_string token))::(tok s (Str.match_end()))

    else if(Str.string_match re_id s pos) then 
    let token = Str.matched_string s in (Tok_ID token)::(tok s (Str.match_end()))

    else if(Str.string_match re_string s pos) then 
    let token = Str.matched_string s in (Tok_String (String.sub token 1 ((String.length token)-2)))::(tok s (Str.match_end()))

    else if(Str.string_match re_space s pos) then (tok s (Str.match_end()))

    else raise (InvalidInputException "InvalidInputException") 
    
in tok input 0