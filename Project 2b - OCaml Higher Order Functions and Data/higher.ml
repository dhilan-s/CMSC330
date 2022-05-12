open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e = fold (fun a x -> if a = true then true else x = e) false lst;;

let is_present lst x = fold (fun a n -> if n = x then a@[1] else a@[0]) [] lst;;

let count_occ lst target = fold (fun a x -> if x = target then a+1 else a) 0 lst;;

let uniq lst = fold (fun a n -> if (contains_elem a n = false) then n::a else a) [] lst;;

let assoc_list lst = fold (fun a n -> (n, count_occ lst n)::a) [] (uniq lst);;

let ap fns args = fold (fun a x -> a@(map x args)) [] fns;;
