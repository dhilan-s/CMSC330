(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = match tup with (a, b, c) -> (c, b, a);;

let is_odd x = if x mod 2 = 0 then false else true;;

let area x y = match x, y with (a, b), (c, d) -> 
  let l = c-a in 
  let w = d-b in 
  abs (l*w);;

let volume x y = match x, y with (a, b, c), (d, e, f) -> 
  let l = d-a in 
  let w = e-b in 
  let h = f-c in
  abs (l*w*h);;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = if n = 0 then 0 else
  if n = 1 then 1 else
    fibonacci(n-1)+fibonacci(n-2);;

let rec pow x y = if y = 0 then 1 else
  if y = 1 then x else
  x * pow x (y-1);;

let rec log x y = if y/x = 0 then 0 else
  1 + log x (y/x);;

let rec gcf x y = if y = 0 then x else gcf y (x mod y);;

let rec prime_helper x i = 
  i*i > x || (x mod i != 0 && prime_helper x (i+1));;

let rec is_prime x = if x <= 1 then false else
  prime_helper x 2;;

(*****************)
(* Part 3: Lists *)
(*****************)
let rec length l = match l with
  | [] -> 0
  | (_::t) -> 1 + (length t);;

let rec append l m = match l with
  | [] -> m
  | (x::xs) -> x::(append xs m);;

let rec revhelper lst = match lst with
| [] -> []
| (x::xs) -> append (revhelper xs) (x::[]);;

let rec get idx lst = match lst with 
| [] -> failwith "Out of bounds"  
| head::rest -> 
    if idx < 0 then failwith "Out of bounds" else 
      if idx = 0 then head else 
      get (idx-1) rest;;


let larger lst1 lst2 = let l1 = length lst1 in
        let l2 = length lst2 in
        if l1 < l2 then lst2 else 
        if l1 > l2 then lst1 else [];;

let reverse lst = revhelper lst;;

let rec combine lst1 lst2 = append lst1 lst2;;

let rec merge lst1 lst2 = match lst1, lst2 with
| [], [] -> []
| [], _ -> lst2
| _, [] -> lst1
| e1::l1, e2::l2 -> 
  if e1 < e2 then e1 :: merge l1 lst2 else
     e2 :: merge lst1 l2;;

let rec rotate shift lst = if shift = 0 then lst else
  match lst with
  | [] -> []
  | first :: rest -> 
    let x = first::[] in
    rotate (shift-1) (append rest x);;

let rec is_palindrome lst = lst = reverse lst;;