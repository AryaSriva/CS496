open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds
(*Name: Aryaman Srivastava
   CS496 HW3
   I pledge my honor that I have abided by the Stevens Honors System*)    
(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> exp_val ea_result =
  fun e ->
  match e with
  | Int(n) ->
    return (NumVal n)
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1+n2))
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1-n2))
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1*n2))
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return (NumVal (n1/n2))
  | Let(id,def,body) ->
    eval_expr def >>= 
    extend_env id >>+
    eval_expr body 
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return (BoolVal (n = 0))
  | Pair(e1,e2) ->
    eval_expr e1 >>= fun ev1 ->
    eval_expr e2 >>= fun ev2 ->
    return (PairVal(ev1,ev2))
  | Fst(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun (l,_) ->
    return l
  | Snd(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun (_,r) ->
    return r
  | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str; 
    error "Debug called"
  | EmptyTree(_t) -> 
    return (TreeVal Empty)
  | Node(e1, e2, e3) ->
    eval_expr e1 >>= fun e1 ->
    eval_expr e2 >>= 
    tree_of_treeVal >>= fun e2 ->
    eval_expr e3 >>= 
    tree_of_treeVal >>= fun e3 ->
    return (TreeVal(Node(e1, e2, e3)))
  | IsEmpty(e) ->
    eval_expr e >>= 
    tree_of_treeVal >>= fun e ->
    return (BoolVal (e = Empty))
  | CaseT (e1, e2, id1, id2, id3, e3 ) ->
    eval_expr e1 >>=
    tree_of_treeVal >>= fun e1 ->
    (match e1 with
    | Empty -> 
      eval_expr e2 >>= fun e2 -> 
      return e2
    | Node(data, lt, rt) ->
      extend_env id1 data >>+
      extend_env id2 (TreeVal lt) >>+ 
      extend_env id3 (TreeVal rt) >>+
      eval_expr e3 >>= fun e3 ->
      return e3)
  | Record(fs) -> 
    let stringList = (fst(List.split(fs)))
  in 
    if (containsDuplicates stringList) then error "Record: duplicate fields" else 
    eval_exprs (snd(List.split(snd(List.split(fs))))) >>= fun fs ->
    let record = List.combine stringList fs
    in
      return (RecordVal record)
  | Proj(e, id) -> 
    eval_expr e >>=
    record_of_recordVal >>= fun e ->
    let rec projHelper: (string*exp_val) list -> string -> exp_val ea_result =
      fun es id -> 
        match es with 
        | [] -> error "Proj: field does not exist"
        | (s, i)::es -> if s = id then return i else projHelper es id
    in projHelper e id
  | _ -> failwith "Not implemented yet!"
and 
  eval_exprs: expr list -> (exp_val list) ea_result =
  fun es ->
  match es with 
  | [] -> return []
  | h::t -> eval_expr h >>= fun i ->
    eval_exprs t >>= fun l ->
      return (i::l)
(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e


(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c
  


