(* 
     Quiz 2 - 7 Feb 2024
     Name1: Aryaman Srivastava
     Name2:
 *)

type 'a bt = Empty |  Node of 'a*'a bt*'a bt

(* Helper function that builds a leaf *)
let leaf n = Node(n,Empty,Empty)

(* Two sample binary trees.
   The first one is a BST, but not the second one *)
let t1 : int bt = Node(12,
                       Node(7,Empty,leaf 10),
                       Node(24,
                            leaf 14,
                            Empty))

let t2 : int bt = Node(12,
                       leaf 7,
                       Node(24,
                            leaf 30,
                            Empty))

(** returns smallest element in non-empty tree [t].
    Fails if [t] is empty, it should fail. 
    Note: the tree [t] is not assumed to be a bst *)
let rec mint : 'a bt -> 'a =
  fun t ->
  match t with 
  | Empty -> failwith "tree cannot be empty"
  | Node(data, Empty, Empty) -> data
  | Node(data, lt, rt) -> min (min data (mint lt)) (mint rt)
  
(** returns largest element in non-empty tree [t].
    Fails if [t] is empty, it should fail. 
    Note: the tree [t] is not assumed to be a bst *)
let rec maxt : 'a bt -> 'a =
  fun t ->
  match t with 
  | Empty -> failwith "tree cannot be empty"
  | Node(data, Empty, Empty) -> data
  | Node(data, lt, rt) -> max (max data (maxt lt)) (maxt rt)

(** [is_bst t] determines whether the binary tree [t] is a binary search tree *)
let rec is_bst : 'a bt -> bool =
  fun t ->
  match t with 
  | Empty -> false
  | Node(data, Empty, Empty) -> true
  | Node(_, lt, rt) when lt < rt && lt < t && t < rt -> is_bst lt && is_bst rt
  | _ -> false

(** [add k t] adds key [k] to the bst [t]. 
    Should fail with failwith if [k] is already in the tree.
    Otherwise, returns updated tree *)
let rec add : 'a -> 'a bt -> 'a bt =
  fun k t ->
  match t with 
  | Empty -> Node(k, Empty, Empty)
  | Node(d, Empty, Empty) when k < d -> Node(d, Node(k, Empty, Empty), Empty)
  | Node(d, Empty, Empty) when k > d -> Node(d, Empty, Node(k, Empty, Empty))
  | Node(d, _, _) when d = k -> failwith "node is already in the tree"
  | Node(d, lt, rt) when k < d -> add k lt
  | Node(d, lt, rt) -> add k rt
           
(** [rem k t] removes key [v] from the BST [t] 
   Should fail with failwith if [k] is not in tree
   (Extra-credit) *)
let rec rem : 'a -> 'a bt -> 'a bt =
   fun k t ->
   failwith "implement"
 





