(*

    3 Feb 2024
    Some exercises on binary trees

*)
type 'a bt = Empty | Node of 'a*'a bt*'a bt

(*
    33
   /  \
  12  77
     /  \
    44  102
*)
let t1: int bt =
  Node(33,
       Node(12,Empty,Empty),
       Node(77,
            Node(44,Empty,Empty),
            Node(102,Empty,Empty)))
(*
    33
   /  \
  12  77
     /  \
    44  102
          \
          777
            \
            1027
*)
let t2: int bt =
  Node(33,
       Node(12,Empty,Empty),
       Node(77,
            Node(44,Empty,Empty),
            Node(102,Empty,
                Node(777,
                     Empty,
                     Node(1027,Empty,Empty)))))

(** [mem e t] returns a boolean indicating whether [e] is in [t] *)
let rec mem e t =
  match t with 
  | Empty -> false
  | Node(a, lt, rt)::t when a = e -> true
  | Node(a, lt, rt)::t when e < a -> mem e lt
  | _::t -> mem e rt

(** [remove e t] removes all leaves in [t] whose data is [e].
    If [e] is not a leaf, then it should fail.
    Eg. remove 102 t1 => Node (33, Node (12, Empty, Empty), Node (77,
    Node (44, Empty, Empty), Empty))  
*)    
let rec remove e t =
  match t with 
  | Empty -> Empty
  | Node(d, Empty, Empty) when d = e -> Empty
  | Node(d, lt, rt) when d = e -> failwith "not a leaf"
  | Node(d,lt,rt) -> Node(d, remove e lt, remove e rt)
    

(** [prune_at_level i t] prunes [t] at level [i].
    Precondition: [i] is positive.
   Eg. prune_at_level 0 t1 => Empty
   Eg. prune_at_level 1 t1 => Node (33, Empty, Empty)
   Eg. prune_at_level 2 t1 => Node (33, Node (12, Empty, Empty), Node
    (77, Empty, Empty))
   Eg. prune_at_level 20 t1 => Node (33, Node (12, Empty, Empty),
 Node (77, Node (44, Empty, Empty), Node (102, Empty, Empty)))
  HINT: match on both i and t
*)
let rec prune_at_level i t =
   failwith "implement"

(** [height t] returns the height of [t].
    Eg. height t1 => 3 
    Eg. height t2 => 5 *)
let rec height t =
   match t with 
   | Empty -> 0 
   | (data, lt, rt)::t -> 1 + height lt + height rt


(** [is_balanced t] returns a boolean indicating whether [t] is balanced.
    A tree is balanced if each node is balanced.
    A node is balanced if the difference in height between its
    children is less than 2.
    Eg. is_balanced t1 => true 
    Eg. is_balanced t2 => false *)
let rec is_balanced t =
   match t with 
   | Empty -> true
   | (data, lt, rt)::t when height lt - height rt <= 2 || height rt - height lt <= 2 -> is_balanced lt && is_balanced rt
   | _ -> false



(* You can skip this part if you do not remember about AVL trees *)
     
type bal = Bal | RR | RL | LL | LR

(** [balance_type t] determines the balance type of [t] *)
let balance_type : 'a bt -> bal =
  fun t ->
 failwith "implement"
        
let rec rotate_left t =
  match t with
  | Node(d1,lt1,Node(d2,lt2,rt2)) -> failwith "implement"
  | _ -> failwith "rotate_left: invalid input"

let rec rotate_right t =
  match t with
  | Node(d1,Node(d2,lt1,rt1),rt2) ->  failwith "implement"
  | _ -> failwith "rotate_left: invalid input"

let rec balance t =
  match t with
  | Empty -> Empty
  | Node(d,lt,rt) ->
    let blt = balance lt
    in let brt = balance rt
    in let res = Node(d,blt,brt)
    in failwith "implement"

                 
