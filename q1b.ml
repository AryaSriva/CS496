(* Quiz 1 - 31 January 2024

   Student name 1: Aryaman Srivastava
   Student name 2:
   Pledge: I pledge my honor that I have abided by the Stevens Honors System
*)


(* Notes: 
    a. You may add helper functions.
    b. "let rec" allows your function to be recursive, but it doesn't
    have to be. 
*)

(* Sample Tree *)
let ex = [(12, 7); (12, 43); (7, 4); (43, 33); (43,77)]

(*
      12
      /\ 
     /  \  
    7   43
   /    /\ 
  /    /  \  
 4    33  77
*)

(** [sub l1 l2] returns the list resulting from subtracting every 
    element in [l2] from [l1].
    Eg. sub [1;2] [] => [1; 2]
    Eg. sub [1;2;1] [1] => [2]
    Eg. sub [1;2] [2;3;1] => []
*)
let rec sub l1 l2 =
  match l1 with
  | [] -> []
  | x::l1 when List.mem x l2 = false -> [x] @ sub l1 l2
  | _::l1 -> sub l1 l2
    
(** [outgoing_nodes t n] returns the list of nodes outgoing from node
 ** [n] in the tree [t]. You may assume the node [n] exists in the
 ** tree [t] .
 ** Eg. outgoing_nodes ex 12 => [7; 43]
*)
let rec outgoing_nodes t (n:int) =
  match t with 
  | [] -> []
  | (x, y)::t when x = n -> [y] @ outgoing_nodes t n
  | _::t -> outgoing_nodes t n
    
(**  [nodes t] returns the list of nodes of the tree without
    duplicates. The order of the
   nodes in the list is irrelevant.
   eg. nodes ex => [12; 7; 4; 33; 43; 77]

*)   
let rec nodes t =
  let rec nodesHelper t = 
    match t with
    | [] -> []
    | (x,y)::t -> [x] @ [y] @ nodesHelper t
  in let tree = nodesHelper t 
  match tree with
  | [] -> []
  | x::tree when List.mem x tree = false -> [x] @ nodes tree
  | _::tree -> nodes tree
  
    
(** [leaves t] returns the leaves of the tree [t]
   Eg. leaves ex =>  [4; 33; 77]
*)
let rec leaves t =
  let rec leavesHelper t = 
    match t with 
    | [] -> []
    | (x, y)::t -> [y] @ leavesHelper t
  in List.filter() *)
(* 
   Returns the root of a tree
   Eg. root ex =>  [12]

let rec root t =
  failwith "complete"

(* 
   Returns the boolean indicating if the tree is a binary tree.
   Eg. is_binary ex =>  true
*)
let rec is_binary t =
  failwith "complete"

(** [subtree t n] returns the subtree rooted at node [n]. (extra-credit)
 **  Eg. subtree ex 12 => [(43, 33); (43, 77); (7, 4); (12, 7); (12, 43)]
         subtree ex 43 -=> [(43, 33); (43, 77)]
         subtree ex 7 => [(7, 4)]
         subtree ex 4 => []
*)
let rec subtree t (n:int) =
  failwith "complete"

                               

