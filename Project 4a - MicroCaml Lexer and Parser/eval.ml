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
| Value x -> eval_value x 
| ID x -> lookup env x
| Fun (x,y)-> (Closure(env,x,y))
| Not a -> eval_not env a
| Binop (a,b,c) -> eval_binop a b c env
| If (a,b,c) -> eval_if a b c env 
| FunctionCall (a,b) -> eval_functioncall a b env 
| Let(a,b,c,d) -> eval_let a b c d env

and eval_value env = 
  match env with 
  | Int a -> (Int a)
  | Bool b -> (Bool b)
  | String s -> (String s)
  | Closure (a,b,c) -> Closure(a,b,c) 


and eval_not env e = match (eval_expr env e) with
  | Bool a -> Bool (not a)
  | _ -> raise (TypeError "not failure")

and eval_binop a b c env = match a with
  | Add -> (match (eval_expr env b) with
          | Int x -> (match (eval_expr env c) with 
                | Int y -> Int(x+y)
                | _ -> raise(TypeError "add failure")
          )
          | _ -> raise(TypeError "add failure"))
          
          
  | Sub ->  (match (eval_expr env b) with
          | Int x -> (match (eval_expr env c) with 
                | Int y -> Int(x-y)
                | _ -> raise(TypeError "sub failure")
            )
          | _ -> raise(TypeError "sub failure"))
          
      
  | Mult -> (match (eval_expr env b) with
          | Int x -> (match (eval_expr env c) with 
                | Int y -> Int(x*y)
                | _ -> raise(TypeError "mult failure")
          )
          | _ -> raise(TypeError "mult failure")
  )

  | Div -> (match (eval_expr env b) with
          | Int x -> (match (eval_expr env c) with 
                | Int y -> if y = 0 then raise(DivByZeroError) else  Int(x/y)
                | _ -> raise(TypeError "div failure")
          )
          | _ -> raise(TypeError "div failure")
  )
  | Greater -> (match (eval_expr env b) with
          | Int x -> 
            (match (eval_expr env c) with 
                | Int y -> Bool (x > y)
                | _ -> raise(TypeError "greater failure")
            )
          | _ -> raise(TypeError "greater failure")
  )
  
  | Less -> (match (eval_expr env b) with
          | Int x -> (match (eval_expr env c) with 
                | Int y -> Bool (x < y)
                | _ -> raise(TypeError "less failure")
          )
          | _ -> raise(TypeError "less failure")
  )

  | LessEqual -> (match (eval_expr env b) with
          | Int x -> (match (eval_expr env c) with 
                | Int y -> Bool (x <= y)
                | _ -> raise(TypeError "lessequal failure")
          )
          | _ -> raise(TypeError "lessequal failure")
  )
        
  | GreaterEqual -> (match (eval_expr env b) with
          | Int x -> (match (eval_expr env c) with 
                | Int y -> Bool (x >= y)
                | _ -> raise(TypeError "geq failure")
          )
          | _ -> raise(TypeError "geq failure")
  )

  | Concat -> (match (eval_expr env b) with
          | String x -> (match (eval_expr env c) with 
                | String y -> String (x^y)
                | _ -> raise(TypeError "string failure")
          )
          | _ -> raise(TypeError "string failure")
  )

  | Equal -> (match (eval_expr env b) with
          | Int x -> (match (eval_expr env c) with 
                | Int y -> Bool (x = y)
                | _ -> raise(TypeError "equal failure")
          )
          | String x -> (match (eval_expr env c) with 
                | String y -> Bool (x = y)
                | _ -> raise(TypeError "equal failure")
          )
          | Bool x -> (match (eval_expr env c) with 
                | Bool y -> Bool (x = y)
                | _ -> raise(TypeError "equal failure")
          )
          | _ -> raise(TypeError "equal failure")
  )

  | NotEqual -> (match (eval_expr env b) with
          | Int x -> (match (eval_expr env c) with 
                | Int y -> Bool (x != y)
                | _ -> raise(TypeError "notequal failure")
          )
          | String x -> (match (eval_expr env c) with 
                | String y -> Bool (x != y)
                | _ -> raise(TypeError "notequal failure")
          )
          | Bool x -> (match (eval_expr env c) with 
                | Bool y -> Bool (x != y)
                | _ -> raise(TypeError "nequal failure")
          )
          | _ -> raise(TypeError "nequal failure")
  )

  |Or -> (match (eval_expr env b) with
          | Bool x -> (match (eval_expr env c) with 
                | Bool y -> Bool (x || y)
                | _ -> raise(TypeError "or failure")
          )
          | _ -> raise(TypeError "or failure")
  )

  |And -> (match (eval_expr env b) with
          | Bool x -> (match (eval_expr env c) with 
                | Bool y -> Bool (x && y)
                | _ -> raise(TypeError "and failure")
          )
          | _ -> raise(TypeError "and failure")
  )
        
and eval_if a b c env = 
let value =  (eval_expr env a) in
  if value = (Bool true) then (eval_expr env b) else if value = (Bool false) then (eval_expr env c) else raise(TypeError " if failure")

and eval_functioncall a b env = match (eval_expr env a) with 
| Closure(x,y,z) -> (eval_expr (extend x y (eval_expr env b)) z)
| _ -> raise(TypeError "functioncall error")

and eval_let a b c d env = 
  if b = false then 
    let x = eval_expr env c in 
      let y = extend env a x in
        eval_expr y d
    else 
    let x = extend_tmp env a in 
    let y = eval_expr x c in 
      let _ = update x a y in 
      eval_expr x d

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with 
| Def (x, y) -> let a = extend_tmp env x in let b = eval_expr a y in let _ = update a x b in (a, Some b)
| Expr x -> let y = eval_expr env x in (env, Some y)
| NoOp -> (env, None)