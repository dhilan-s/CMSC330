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
    | Some Tok_Let -> parse_letexpr toks
    | Some Tok_Fun -> parse_functionexpr toks
    | Some Tok_If -> parse_ifexpr toks
    | _ -> parse_orexpr toks

and parse_letexpr toks =
  let tok = match_token toks Tok_Let in
  let is_rec = if (lookahead tok) = (Some Tok_Rec) then true else false in
  let next_tokens = if is_rec then match_token tok (Tok_Rec) else tok in
  
  match lookahead next_tokens with
  | Some Tok_ID id ->
    let tok_val = match_token next_tokens (Tok_ID id) in
    let equal = match_token tok_val (Tok_Equal) in
    let (a, b) = parse_expr equal in
    let in_val = match_token a (Tok_In) in
    let (x, y) = parse_expr in_val in
    (x, (Let (id, is_rec, b, y)))
  | _ -> raise (InvalidInputException "No ID in Let")

and parse_ifexpr toks = 
  let if_val = match_token toks Tok_If in
  let (a, b) = parse_expr if_val in
  let then_val = match_token a (Tok_Then) in
  let (x, y) = parse_expr then_val in 
  let else_val = match_token x (Tok_Else) in
  let (w, z) = parse_expr else_val in
  (w, If(b, y, z))

and parse_functionexpr toks =
    let next_tokens = match_token toks(Tok_Fun) in

    let id = match lookahead next_tokens with
    | Some Tok_ID tok -> tok
    | _ ->  raise (InvalidInputException "No ID in Fun")
    in

    let next = match_token next_tokens (Tok_ID id) in 
    let arrow = match_token next (Tok_Arrow) in
    let (a,b) = parse_expr arrow in 
    (a, (Fun (id, b)))

and parse_orexpr toks = 
    let (a,b) = parse_andexpr toks in
    let id = lookahead a in
    if id = Some Tok_Or then let (c,d) = parse_orexpr (match_token a Tok_Or) in (c, Binop(Or, b, d))
    else (a,b)

and parse_andexpr toks = 
  let (a,b) = parse_equalityexpr toks in
  let id = lookahead a in
  if id = (Some Tok_And) then let (c, d) = parse_andexpr (match_token a Tok_And) in (c, Binop(And, b, d))
  else (a,b)

and parse_equalityexpr toks = 
  let (a,b) = parse_relationalexpr toks in 
  let x = lookahead a in if x = (Some Tok_Equal) then let (c,d) = parse_equalityexpr (match_token a (Tok_Equal)) in (c, Binop(Equal, b, d))
  else if x = (Some Tok_NotEqual) then let (c,d) = parse_equalityexpr (match_token a (Tok_NotEqual)) in (c, Binop(NotEqual, b, d))
  else (a,b)

and parse_relationalexpr toks =
let (a, b) = parse_additiveexpr toks in
match lookahead a with
| Some Tok_Less -> let (c,d) = parse_relationalexpr (match_token a Tok_Less) in (c, Binop(Less, b, d))
| Some Tok_Greater -> let (c,d) = parse_relationalexpr (match_token a Tok_Greater) in (c, Binop(Greater, b, d))
| Some Tok_LessEqual -> let (c,d) = parse_relationalexpr (match_token a Tok_LessEqual) in (c, Binop(LessEqual, b, d))
| Some Tok_GreaterEqual -> let (c,d) = parse_relationalexpr (match_token a Tok_GreaterEqual) in (c, Binop(GreaterEqual, b, d))
| _ -> (a,b)

and parse_additiveexpr toks =
let (a,b) = parse_multiplicativeexpr toks in 
   match lookahead a with 
   | Some Tok_Add -> let (c, d) = parse_additiveexpr (match_token a Tok_Add) in (c, Binop(Add, b, d))
   | Some Tok_Sub -> let (c, d) = parse_additiveexpr (match_token a Tok_Sub) in (c, Binop(Sub, b, d))
   | _ -> (a,b)

and parse_multiplicativeexpr toks =
let (a,b) = parse_concatexpr toks in
  match lookahead a with 
   | Some Tok_Mult -> let (c, d) = parse_multiplicativeexpr (match_token a Tok_Mult) in (c, Binop(Mult, b, d))
   | Some Tok_Div -> let (c, d) = parse_multiplicativeexpr (match_token a Tok_Div) in (c, Binop(Div, b, d))
   | _ -> (a,b)
  
and parse_concatexpr toks =
let (a,b) = parse_unaryexpr toks in
    let x = lookahead a in if x = (Some Tok_Concat) then let (c,d)  = parse_concatexpr (match_token a Tok_Concat) in (c, Binop(Concat, b, d))
    else (a,b)

and parse_unaryexpr toks = 
let x = lookahead toks in if x = (Some Tok_Not) then let (a,b) = parse_unaryexpr (match_token toks Tok_Not) in (a, Not (b))
    else parse_functioncallexpr toks

and parse_functioncallexpr toks = 
  let (a,b) = parse_primaryexpr toks in 
  match(lookahead a) with 
  | Some Tok_Int x      -> let (c,d)  = parse_primaryexpr a in (c, FunctionCall(b,d)) 
  | Some Tok_Bool x     -> let (c,d)  = parse_primaryexpr a in (c, FunctionCall(b,d))
  | Some Tok_String x   -> let (c,d)  = parse_primaryexpr a in (c, FunctionCall(b,d))
  | Some Tok_ID x     -> let (c,d)  = parse_primaryexpr a in (c, FunctionCall(b,d))
  | Some Tok_LParen     -> let (c,d)  = parse_primaryexpr a in (c, FunctionCall(b,d))
  | Some _ ->(a,b)
  | None -> (a,b)

and parse_primaryexpr toks = 
  match (lookahead toks) with
  | Some Tok_Int a -> match_token toks (Tok_Int(a)), Value(Int a)
  | Some Tok_Bool b -> match_token toks (Tok_Bool(b)), Value(Bool b)
  | Some Tok_String c -> match_token toks (Tok_String(c)), Value(String c)
  | Some Tok_ID d -> match_token toks (Tok_ID(d)), (ID d)
  | Some Tok_LParen -> let (x,y) = parse_expr (match_token toks (Tok_LParen)) in (match_token x Tok_RParen, y)
  | _ -> raise (InvalidInputException "Illegal argument entered")
   
(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
let x = lookahead toks in
if x = (Some Tok_Def) then def_mutop toks
else if (x = Some Tok_DoubleSemi) then (match_token toks Tok_DoubleSemi), NoOp
else if x != None then expr_mutop toks
else raise (InvalidInputException "invalid argument")

and def_mutop toks = 
  let def  = match_token toks (Tok_Def) in 
    match lookahead def with
    | Some Tok_ID id -> let tok = match_token def (Tok_ID(id)) in
      let equals = match_token tok (Tok_Equal) in
      let (a,b) = parse_expr equals in
      let semis = match_token a (Tok_DoubleSemi) in
      (semis, Def(id, b))
    | Some _ -> raise (InvalidInputException "invalid argument")
    | None -> raise (InvalidInputException "invalid argument")

and expr_mutop toks = 
  let (a,b) = parse_expr toks in 
   let semis = match_token a (Tok_DoubleSemi) in
   (semis, (Expr(b)))