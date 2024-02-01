(*Name: Aryaman Srivastava
  Pledge: I pledge my honor that I have abided by the Stevens Honors System
  CS 496 HW 1*)

(* ******************************************** *)
(** Basic functions on finite automata *)
(* ******************************************** *)
(**
   Code stub for assignment 1
*)

type symbol = char
type input = char list

type state = string

(* transition function *)
type tf = (state * symbol * state) list

(* initial state * transition function * end state *)
type fa = { states: state list; start:state; tf: tf; final: state list}


(* ******************************************** *)
(* Examples of automata *)
(* ******************************************** *)

let a = {states = ["q0";"q1";"q2"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
         final = ["q2"]}

let a2 = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1")
               ; ("q1",'c',"q2");  ("q3",'a',"q4")];
          final= ["q2"]
         }

let a3 = {states = ["q0";"q1";"q2";"q3"];
          start = "q0";
          tf = [("q0", 'a', "q1"); ("q1", 'b', "q1"); ("q1", 'c', "q2"); ("q0", 'a', "q3")];
          final = ["q2"]
         }

let a4 = {states = ["q0";"q1";"q2";"q3"];
          start = "q0";
          tf = [("q0", 'a', "q1"); ("q1", 'b', "q1"); ("q1", 'c', "q2"); ("q3", 'a', "q4")];
          final = ["q3"]
         }

let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]



(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


let rec removeDuplicateStates states = (*removes duplicate states from the given state list*)
  match states with 
  | [] -> []
  | state::states when List.mem state states = false -> [state] @ removeDuplicateStates states
  | _::states -> removeDuplicateStates states

(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)

(*applies the transition function to the current state with given symbol*)
let apply_transition_function tf' state' symbol' =
  let rec apply_transition_function_helper tf' state' symbol' =
    match tf' with
    | [] -> None
    | (x,y,z)::tf' when state' = x && symbol' = y -> Some z
    | _::tf' -> apply_transition_function_helper tf' state' symbol'
  in 
  apply_transition_function_helper tf' state' symbol'

(*function to see if the given full automaton accepts the given word given as a list*)
let accept fa' input = 
  let rec acceptHelper fa input state = (*recurses over input and repeatedly applies transition function to see if we get to a valid state*)
    match input with
    | [] -> state
    | symbol::input -> 
      match state with 
      | Some z -> acceptHelper fa input (apply_transition_function fa.tf z symbol)
      | None -> None
  in
  match acceptHelper fa' input (Some fa'.start) with (*once out of the helper function, check that we actually reached a state and if that state is an end state*)
  | Some state -> List.mem state fa'.final
  | None -> false

(* function to get all successor states from the current state with the symbol *)
let next tf' state' symbol' =
  let rec nextHelper tf' state' symbol' =
    match tf' with
    | [] -> []
    | (x,y,z)::tf' when state' = x && symbol' = y -> [z] @ nextHelper tf' state' symbol'
    | _ :: tf' -> nextHelper tf' state' symbol'
  in 
  nextHelper tf' state' symbol'

(*function to determine whether a full automaton is deterministic*)
let is_deterministic fa' = 
  let rec is_deterministic_helper tf =
    match tf with
    | [] -> true
    | (x,y,z)::tf' when List.length(next tf x y) <= 1 -> is_deterministic_helper tf'
    | _ -> false
  in 
  is_deterministic_helper fa'.tf

(*function to determine whether a full automaton is valid*)
let valid fa'=
  let rec validHelper states =
    match states with
    | [] -> true
    | state::states when List.mem state states -> false
    | _::states -> validHelper states
  in 
  validHelper fa'.states && is_deterministic fa'&& List.mem fa'.start fa'.states && List.mem (List.hd fa'.final) fa'.states

(*function to return all states which are reachable from the start state in the full automaton*)
let reachable fa' =
  let rec reachableHelper2 tf state = (*recurses over all the states in the transition function calling the helper function to get a list of reachable states*)
    let rec reachableHelper tf state = (*gets all the reachable states from the given state*)
      match tf with
      | [] -> []
      | (x,y,z)::tf when state = x -> [z] @ reachableHelper tf z
      | _::tf -> reachableHelper tf state
    in 
    match tf with 
    | [] -> []
    | (x,y,z)::tf when state = x -> [state] @ reachableHelper tf x @ reachableHelper2 tf z
    | _::tf -> reachableHelper2 tf state
  in 
  removeDuplicateStates (reachableHelper2 fa'.tf fa'.start)

(*function to check if a full automaton is non-empty (at least one final state is reachable)*)
let non_empty fa'=
  let rec non_empty_helper fa' final =
    match final with 
    | [] -> false
    | state::final when List.mem state (reachable fa') = true -> true
    | _::final -> non_empty_helper fa' final 
  in
  non_empty_helper fa' fa'.final


(*function to remove all states which are not reachable from the start state from the transition function, final state list and state list*)
let remove_dead_states fa' = 
  let rec dead_states_helper2 tf fa' = (*function to remove transitions with unreachable states from the transition function*)
    match tf with
    | [] -> []
    | (x,y,z)::tf when List.mem x (reachable fa') = true && List.mem z (reachable fa') = true -> [(x,y,z)] @ dead_states_helper2 tf fa'
    | _::tf -> dead_states_helper2 tf fa'
  in
  let rec dead_states_helper state_list fa' = (*function to remove unreachable states from given state list*)
    match state_list with
    | [] -> [] 
    | state::state_list when List.mem state (reachable fa') = true -> [state] @ dead_states_helper state_list fa'
    | _::state_list -> dead_states_helper state_list fa'
  in
  { (*return a new fa at the end of the function*)
    states = dead_states_helper fa'.states fa';
    start = fa'.start;
    tf = dead_states_helper2 fa'.tf fa';
    final = dead_states_helper fa'.final fa'
  }