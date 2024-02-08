(* 
  Name: Aryaman Srivastava
  Pledge: I pledge my honor that I have abided by the Stevens Honors System
  CS496 HW2 *)

type dTree = 
| Leaf of int 
| Node of char*dTree*dTree

let tLeft = Node('w', Node('x', Leaf(2), Leaf(5)), Leaf(8))
let tRight = Node('w', Node('x', Leaf(2), Leaf(5)), Node('y', Leaf(7), Leaf(5)))
let graph = [([0;0;0] , 0);
([0;0;1] , 1);
([0;1;0] , 1);
([0;1;1] , 0);
([1;0;0] , 1);
([1;0;1] , 0);
([1;1;0] , 0);
([1;1;1] , 1)]

let rec height : dTree -> int =
  (* function to calculate height of given tree *)
  fun tree ->
    match tree with
    | Leaf(data) -> 1
    | Node(data, lt, rt) -> max (1 + height lt) (1 + height rt)

let rec size: dTree -> int = 
  (* function to calculate size(number of nodes) in given tree *)
  fun tree ->
    match tree with
    | Leaf(data) -> 1
    | Node(data, lt, rt) -> 1 + size lt + size rt

let rec paths: dTree -> int list list =
  (* function to return a list with all paths to leaves *)
  fun tree ->
    match tree with
    | Leaf(data) -> [[]]
    | Node(data, lt, rt) -> (List.map (fun lst -> 0::lst) (paths lt)) @ (List.map (fun lst -> 1::lst) (paths rt))

let rec is_perfect: dTree -> bool =
  (* function to return if a given tree is perfect *)
  fun tree ->
    match tree with
    | Leaf(data) -> true
    | Node(data, lt, rt) ->
      if (height lt) != height rt then false else (is_perfect lt) && (is_perfect rt)

let rec map: (char -> char) -> (int -> int) -> dTree -> dTree =
  (* function to map all nodes in the given tree with the given function *)
  fun f g t ->
    match t with
    | Leaf(data) -> Leaf(g data)
    | Node(data, lt, rt) -> Node(f data, map f g lt, map f g rt)

let rec list_to_tree: char list -> dTree =
  (* function to turn the given list into a tree *)
  fun lst ->
    match lst with
    | [] -> Leaf(0)
    | x::lst -> Node(x, list_to_tree lst, list_to_tree lst)

let rec replace_leaf_at: dTree -> (int list * int) list -> dTree = 
  (* function to replace the leaves of the tree with the value specified in the given graph *)
  fun tree graph ->
    let rec helper: dTree -> int list -> int -> dTree =
      fun tree list value ->
        match list with
        | [] -> Leaf(value)
        | x::list -> 
          match tree with 
          | Node(data, lt, rt) -> 
            if x = 0 then Node(data, helper lt list value, rt) else Node(data, lt, helper rt list value)
    in
    match graph with 
    | [] -> tree
    | (lst, value)::graph -> replace_leaf_at (helper tree lst value) graph

let bf_to_dTree: char list * (int list * int) list -> dTree =
    (* function to turn a pair encoding of a boolean function into a tree encoding *)
  fun bf ->
    match bf with
    | (x,y) -> replace_leaf_at (list_to_tree x) y
