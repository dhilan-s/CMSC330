open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with 
  | IntLeaf -> IntNode (x, None, empty_int_tree, empty_int_tree, empty_int_tree)
  | IntNode (num, Some numtwo, left, center, right) -> 
    if x < num then 
      IntNode (num, Some numtwo, (int_insert x left), center, right) else
    if x > num && x < numtwo then 
      IntNode (num, Some numtwo, left, (int_insert x center), right) else 
    if x > numtwo then 
      IntNode (num, Some numtwo, left, center, (int_insert x right)) else
      IntNode (num, Some numtwo, left, center, right)
  | IntNode (num, None, left, center, right) -> 
    if x < num then 
      IntNode (num, None, (int_insert x left), center, right) else
    if x > num then
        IntNode (num, Some x, left, center, right) else 
        IntNode (num, None, left, center, right);;

let rec int_mem x t =
  match t with 
  | IntLeaf -> false 
  | IntNode (num, Some numtwo, left, center, right) -> 
    if x = num || x = numtwo then true else
    if x < num then int_mem x left else
    if x > num && x < numtwo then int_mem x center else 
      int_mem x right
  | IntNode (num, None, left, center, right) -> 
    if x = num then true else
    if x < num then int_mem x left else
     int_mem x right;; 

let rec int_size t =
  match t with 
  | IntLeaf -> 0
  | IntNode (num, Some numtwo, left, center, right) -> 
    2 + int_size left + int_size right + int_size center
  | IntNode (num, None, left, center, right) -> 
    1 + int_size left + int_size right + int_size center;;

let rec int_max t =
  match t with
  | IntLeaf -> raise (Invalid_argument "int_max on an empty tree")
  | IntNode (num, Some numtwo, left, center, right) -> 
    if right = IntLeaf then numtwo else
    int_max right
  | IntNode (num, None, left, center, right) -> 
    if right = IntLeaf then num else
      int_max right;;

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_put k v t = 
  match t with 
   | MapLeaf -> MapNode ((k,v), None, empty_tree_map, empty_tree_map, empty_tree_map)
  | MapNode ((k1, v1), Some (k2, v2), left, center, right) -> 
    if k < k1 then 
      MapNode ((k1,v1), None, (map_put k v left), center, right) else
    if k > k1 && k < k2 then 
      MapNode ((k1, v1), Some (k2, v2), left, (map_put k v center), right) else 
     MapNode ((k1, v1), Some (k2, v2), left, center, (map_put k v right))
  | MapNode ((k1, v1), None, left, center, right) -> 
    if k < k1 then 
      MapNode ((k1, v1), None, (map_put k v left), center, right) else
        MapNode ((k1, v1), Some (k, v), left, center, right);;

let rec map_contains k t = 
  match t with 
  | MapLeaf -> false 
  | MapNode ((k1, v1), Some (k2, v2), left, center, right) -> 
    if k < k1 then 
      map_contains k left else
    if k > k1 && k < k2 then 
      map_contains k center else 
    if k > k2 then
     map_contains k right else 
      true
  | MapNode ((k1, v1), None, left, center, right) -> 
    if k < k1 then 
      map_contains k left else
    if k > k1 then 
      map_contains k right else 
      true;;

let rec map_get k t =
  match t with 
  | MapLeaf -> raise (Invalid_argument "map_get") 
  | MapNode ((k1, v1), Some (k2, v2), left, center, right) -> 
    if k < k1 then 
      map_get k left else
    if k > k1 && k < k2 then 
      map_get k center else 
    if k > k2 then
     map_get k right else 
    if k = k1 then v1 else v2 
  | MapNode ((k1, v1), None, left, center, right) -> 
    if k < k1 then 
      map_get k left else
    if k > k1 then 
      map_get k right else 
    v1;;

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = ((string * int) list) list

let empty_table : lookup_table = []

let push_scope (table : lookup_table) : lookup_table = 
  [] :: table;; 

let pop_scope (table : lookup_table) : lookup_table = match table with 
| [] -> failwith "No scopes remain!"
| first :: rest -> rest;;

let add_var name value (table : lookup_table) : lookup_table = match table with 
| [] -> failwith "There are no scopes to add variable to!"
| first :: rest -> if (fold (fun a x -> 
  match x with (k, v) -> if a = true then true else k = name) false first) = true then
    failwith "Duplicate variable binding in scope!" else
      let add:(string * int)  = (name, value) in (add :: first) :: rest;;

let rec lookup name (table : lookup_table) = match table with 
| [] -> failwith "Variable not found!"
| first :: rest -> (fold (fun a x -> 
  match x with (k, v) -> if k = name then v else a) (lookup name (pop_scope table)) first);;