(*
       Fruit Basket Processors
*)

type 'a result = Ok of 'a | Error of string
type fruit = A | O | K
type 'a basket = 'a list

(* Sample fruit baskets *)
let fb1 : fruit basket = [A;A;O;A;K;K]
let fb2 : fruit basket = [A;A;A;A]

(* 
   A fruit basket processor is any expression whose type is an
   instance of:

      fruit basket -> 'a result

   Some examples of types that have this form are: 
   Eg. fruit basket -> int result
   Eg. fruit basket -> bool result
   Eg. fruit basket -> (fruit basket) result

   A fruit basket processor analyzes a fruit basket and can:
   1. Either, fail (returning: Error s, with s a string)
   2. Or, succeed (returning: Ok v, wwith v the result)
*)

(* 
   Implement the following fruit basket processors.
   NOTE: You are free to add the "rec" keyword just after the "let", if needed.
 *)
    
(** [no_of_apples fb] fruit basket processor that returns the number of apples in the fruit basket [fb].
    Eg. no_of_apples fb1 => Ok 3
*)                   
let no_of_apples : fruit basket -> int result =
  fun fb ->
    Ok (List.length (List.filter ((=)A) fb))

(** [no_of_oranges fb] fruit basket processor that returns the number of oranges in the fruit basket [fb].
    Eg. no_of_oranges fb1 => Ok 3
*)                   
let no_of_oranges : fruit basket -> int result =
  fun fb ->
  Ok (List.length (List.filter ((=)O) fb))

(** [no_of_kiwis fb] fruit basket processor that returns the number of kiwis in the fruit basket [fb].
    Eg. no_of_kiwis fb1 => Ok 3
*)                   
let no_of_kiwis : fruit basket -> int result =
  fun fb ->
  Ok (List.length (List.filter ((=)K) fb))

(** [remove_orange fb] fruit basket processor that removes one orange (the first one) from [fb]. If there are no oranges, it should return an error.
    Eg. [remove_orange fb1] => [Ok [A;A;A;K;K]] 
    Eg. [remove_orange fb2] => [Error "no oranges"]
*)
let rec remove_first_orange fb =
  match fb with
  | [] -> assert false
  | O::t -> t
  | h::t -> h :: remove_first_orange t
let remove_orange : fruit basket -> (fruit basket) result =
  fun fb ->
  if List.mem O fb
    then Ok (remove_first_orange fb)
else Error "no oranges"


(** [apples_to_oranges_ratio fb] fruit basket processor that returns
    the quotient between the number of apples and the number of
    oranges in [fb].
    It should return [Error] if there are no oranges. 
    Eg. apples_to_oranges_ratio fb1 => Ok 3
        apples_to_oranges_ratio fb2 => Error
*)
let apples_to_oranges_ratio : fruit basket -> int result =
  fun fb ->
  if List.mem O fb
    then Ok ((List.length (List.filter ((=)A) fb))/(List.length (List.filter ((=)O) fb)))
  else Error "no oranges"


(** [apples_to_kiwis_ratio fb] fruit basket processor that returns
    the quotient between the number of apples and the number of
    kiwis in [fb].
    It should return [Error] if there are no kiwis. 
    Eg. apples_to_kiwis_ratio fb1 => Ok 1
        apples_to_kiwis_ratio fb2 => Error
*)    
let apples_to_kiwis_ratio : fruit basket -> int result =
  fun fb ->
  if List.mem K fb
      then Ok ((List.length (List.filter ((=)A) fb))/(List.length (List.filter ((=)K) fb)))
  else Error "no kiwis"


(** [ratio_sum fb] fruit basket processor that returns the sum of the apples-to-oranges ratio and the apples-to-kiwis ration in [fb].
    IMPORTANT: YOU MUST USE [apples_to_oranges_ratio] AND
    [apples_to_kiwis_ratio] FROM ABOVE.
    Eg. ratio_sum fb1 => Ok 4
        ratio_sum fb2 => Error
*)
let ratio_sum : fruit basket -> int result =
  fun fb ->
  match apples_to_oranges_ratio fb with
  | Error s -> Error s
  | Ok x -> 
    match apples_to_kiwis_ratio fb with
    | Error s -> Error s
    | Ok y -> Ok(x + y) 

let (>>=)
    (c:fruit basket -> 'a result)
    (f: 'a -> fruit basket -> 'a result)
    (fb: fruit basket) =
    match c fb with 
    | Error s -> Error s 
    | Ok m -> f m fb

let ratio_product: fruit basket -> int result = 
      fun fb ->
        match apples_to_oranges_ratio fb with 
        | Error s -> Error s 
        | Ok m -> 
          (match apples_to_kiwis_ratio fb with
          | Error s -> Error s 
          | Ok n -> Ok (m*n))

let ratio_product' = 
  apples_to_oranges_ratio >>= (fun m ->
    apples_to_kiwis_ratio >>= fun n ->
     (fun _fb -> Ok(m*n)))