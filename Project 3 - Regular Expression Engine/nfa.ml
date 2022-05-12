open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []


(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
    List.fold_left (fun a n -> 
    match n with 
    (start, cond, dest) -> if List.mem start qs then 
      if cond = s then insert dest a else a
     else a) [] nfa.delta

let rec e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  List.fold_left (fun a n -> 
    match n with 
    (start, cond, dest) -> if elem start qs then begin
      if cond = None then insert_all (e_closure nfa [dest]) a else a end
     else a) qs nfa.delta

let rec accepthelper (nfa: ('q,'s) nfa_t) (qs: 'q list) chars =
  match chars with  
  | [] -> List.fold_left (fun a n -> if (Sets.elem n qs) then true else a) false nfa.fs
  | head :: tail -> accepthelper nfa (e_closure nfa (move nfa qs (Some head))) tail


  let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
    let chars = explode(s) in 
    let curr = [nfa.q0] in
    let list = (e_closure nfa curr) in
    accepthelper nfa list chars
    

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.fold_left (fun a n -> (insert (e_closure nfa (move nfa qs (Some n))) a)) []  nfa.sigma

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.fold_left (fun a n -> (insert (qs, (Some n), (move nfa qs (Some n))) a)) [] nfa.sigma
     
let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = 
  let there = List.fold_left (fun a n -> if elem n nfa.fs then true else a) false qs in
  if there then [qs] else []

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  match work with 
  | curr :: next -> 
    let added = { qs = (union dfa.qs (new_states nfa curr))
  ; delta = (union dfa.delta (new_trans nfa curr))
  ; fs = (union dfa.fs (new_finals nfa curr))
  ; sigma = dfa.sigma
  ; q0 = dfa.q0} in (nfa_to_dfa_step nfa added next)
  | [] -> dfa


let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let new_dfa = { 
    sigma = nfa.sigma
  ; qs = [e_closure nfa [nfa.q0]]
  ; q0 = e_closure nfa [nfa.q0]
  ; fs = new_finals nfa (e_closure nfa [nfa.q0])
  ; delta = []
  } in
  nfa_to_dfa_step nfa new_dfa [e_closure nfa [nfa.q0]] 
  
