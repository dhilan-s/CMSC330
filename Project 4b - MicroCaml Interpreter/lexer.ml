open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

  let re_rparen = Str.regexp ")" 
  let re_lparen = Str.regexp "(" 
  let re_equal = Str.regexp "="
  let re_notequal = Str.regexp "<>"
  let re_greater = Str.regexp ">"
  let re_less = Str.regexp "<"
  let re_greaterequal = Str.regexp ">="
  let re_lessequal = Str.regexp "<="
  let re_or = Str.regexp "||"
  let re_and = Str.regexp "&&"
  let re_not = Str.regexp "not"
  let re_if = Str.regexp "if"
  let re_then = Str.regexp "then"
  let re_else = Str.regexp "else"
  let re_add = Str.regexp "+"
  let re_sub = Str.regexp "-"
  let re_mult = Str.regexp "*"
  let re_div = Str.regexp "/"
  let re_concat = Str.regexp "\\^"
  let re_let = Str.regexp "let"
  let re_def = Str.regexp "def"
  let re_in = Str.regexp "in"
  let re_rec = Str.regexp "rec"
  let re_fun = Str.regexp "fun"
  let re_arrow = Str.regexp "->"
  let re_doublesemi = Str.regexp ";;"
  let re_int = Str.regexp "[0-9]+"
  let re_bool = Str.regexp "true\\|false"
  let re_string = Str.regexp "\"[^\"]*\""
  let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
  let whitespace = Str.regexp " \\|\t\\|\n"

let tokenize input = 
  let rec tok s pos =
    if pos >= String.length s then
      []
    else

      (* ( *)
      if (Str.string_match re_lparen s pos) 
			  then (Tok_LParen)::(tok s (pos+1))
			
      (* ) *)
		  else if (Str.string_match re_rparen s pos) 
			  then (Tok_RParen)::(tok s (pos+1))
      
      (* = *)
		  else if (Str.string_match re_equal s pos) 
			  then (Tok_Equal)::(tok s (pos+1))
			
      (* <> *)
		  else if (Str.string_match re_notequal s pos) 
			  then (Tok_NotEqual)::(tok s (pos+2))	
			
      (* >= *) 
		  else if (Str.string_match re_greaterequal s pos) 
			  then (Tok_GreaterEqual)::(tok s (pos+2))

      (* <= *)
		  else if (Str.string_match re_lessequal s pos) 
		  	then (Tok_LessEqual)::(tok s (pos+2))
      
      (* > *)
      else if (Str.string_match re_greater s pos) 
		  	then (Tok_Greater)::(tok s (pos+1))
      
      (* < *)
      else if (Str.string_match re_less s pos) 
		  	then (Tok_Less)::(tok s (pos+1))

      (* || *)
      else if (Str.string_match re_or s pos)
        then (Tok_Or)::(tok s (pos+2))

      (* && *)
      else if (Str.string_match re_and s pos)
        then (Tok_And)::(tok s (pos+2))

      (* ;; *)
      else if (Str.string_match re_doublesemi s pos)
        then (Tok_DoubleSemi) :: (tok s (pos+2))

      (* not *)
      else if (Str.string_match re_not s pos)
        then 
          let start = Str.matched_string s in
          let temp = Str.match_end() in
            if Str.string_match re_string s temp then
              let token = start ^ (Str.matched_string s) in
              Tok_ID token::(tok s (pos + (String.length token))) 
            else 
              (Tok_Not)::(tok s (pos+3))
      
      (* if *)       
      else if (Str.string_match re_if s pos)
        then 
          let start = Str.matched_string s in
          let temp = Str.match_end() in
            if Str.string_match re_string s temp then
              let token = start ^ (Str.matched_string s) in
              Tok_ID token::(tok s (pos + (String.length token))) 
            else 
              (Tok_If)::(tok s (pos+2))

      (* then *)
      else if (Str.string_match re_then input pos)
        then 
          let start = Str.matched_string s in
          let temp = Str.match_end() in
            if Str.string_match re_string s temp then
              let token = start ^ (Str.matched_string s) in
              Tok_ID token::(tok s (pos + (String.length token))) 
            else 
              (Tok_Then)::(tok s (pos+4))
         
      (* else *)
      else if (Str.string_match re_else input pos)
        then 
          let start = Str.matched_string s in
          let temp = Str.match_end() in
            if Str.string_match re_string s temp then
              let token = start ^ (Str.matched_string s) in
              Tok_ID token::(tok s (pos + (String.length token))) 
            else 
              (Tok_Else)::(tok s (pos+4))        

      (* -> *)
      else if  (Str.string_match re_arrow s pos)
        then (Tok_Arrow) :: (tok s (pos+2))
     
      (* + *)
      else if  (Str.string_match re_add s pos)
        then (Tok_Add) :: (tok s (pos+1))

      (* - *)
      else if  (Str.string_match re_sub s pos)
        then (Tok_Sub) :: (tok s (pos+1))

      (* * *)
      else if  (Str.string_match re_mult s pos)
        then (Tok_Mult) :: (tok s (pos+1))

      (* / *)
      else if  (Str.string_match re_div s pos)
        then (Tok_Div) :: (tok s (pos+1))

      (* ^ *)
      else if  (Str.string_match re_concat s pos)
        then (Tok_Concat) :: (tok s (pos+1))

      (* let *)
      else if  (Str.string_match re_let s pos)  
        then
          let first = Str.matched_string s in 
          let new_pos = Str.match_end()  in
             if Str.string_match (Str.regexp "[a-zA-Z0-9]+") s (new_pos) then 
                let token = first ^  (Str.matched_string s)in  
                Tok_ID token::(tok input (pos + String.length token))
             else 
          Tok_Let :: (tok s (pos+3))

      (* rec *)
      else if (Str.string_match re_rec s pos)
        then 
          let start = Str.matched_string s in
          let temp = Str.match_end() in
            if Str.string_match re_string s temp then
              let token = start ^ (Str.matched_string s) in
              Tok_ID token::(tok s (pos + (String.length token))) 
            else 
              (Tok_Rec)::(tok s (pos+3))

      (* fun *)
      else if (Str.string_match re_fun s pos)
        then 
          let start = Str.matched_string s in
          let temp = Str.match_end() in
            if Str.string_match re_string s temp then
              let token = start ^ (Str.matched_string s) in
              Tok_ID token::(tok s (pos + (String.length token))) 
            else 
              (Tok_Fun)::(tok s (pos+3))

      (* in *)
      else if (Str.string_match re_in s pos)
        then 
          let start = Str.matched_string s in
          let temp = Str.match_end() in
            if Str.string_match re_string s temp then
              let token = start ^ (Str.matched_string s) in
              Tok_ID token::(tok s (pos + (String.length token))) 
            else 
              (Tok_In)::(tok s (pos+2))

      (* def *)
      else if (Str.string_match re_def s pos)
        then 
          let start = Str.matched_string s in
          let temp = Str.match_end() in
            if Str.string_match re_string s temp then
              let token = start ^ (Str.matched_string s) in
              Tok_ID token::(tok s (pos + (String.length token))) 
            else 
              (Tok_Def)::(tok s (pos+3))

      (* int *)
      else if (Str.string_match re_int s pos)
        then
          let token = Str.matched_string s in 
          Tok_Int (int_of_string token) :: tok s (pos + (String.length token))

      (* bool *)
      else if (Str.string_match re_bool s pos)
        then 
          let start = Str.matched_string s in
          let temp = Str.match_end() in
            if Str.string_match re_string s temp then
              let token = start ^ (Str.matched_string s) in
              Tok_ID token::(tok s (pos + (String.length token))) 
            else 
              Tok_Bool (bool_of_string start)::(tok s (pos+(String.length start)))
      
      (* String *)
      else if (Str.string_match re_string s pos)
        then 
          let str = Str.matched_string s in
          let i = (String.sub str 1((String.length str) -2)) in 
          Tok_String i :: tok s ((pos + String.length i) +3)

      (* ID *)
      else if (Str.string_match re_id s pos)
        then 
          let id = Str.matched_string s in 
          Tok_ID id :: tok s ((pos + String.length id))

      (* Skip White Space *)
      else if (Str.string_match whitespace s pos)
        then tok s (pos + (String.length (Str.matched_string s)))

      else raise (InvalidInputException "tokenize")
  in 
  tok input 0