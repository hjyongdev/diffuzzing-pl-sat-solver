exception Invalid_parameter of string

(* Definition of propositional logic formula *)
type var = string
type formula =
| False
| True
| Var of var
| Not of formula
| And of formula * formula
| Or of formula * formula
| Imply of formula * formula
| Iff of formula * formula

(* @title A function that stringifies a formula
 * @dev The output can be inputted into a function that takes a formula in REPL. *)
let rec formula_to_string f = "(" ^ (
  match f with 
  | True -> "True"
  | False -> "False"
  | Var x -> "Var \"" ^ x ^ "\""
  | Not f -> "Not (" ^ formula_to_string f ^ ")"
  | And (f1, f2) -> "And (" ^ formula_to_string f1 ^ ", " ^ formula_to_string f2 ^ ")"
  | Or (f1, f2) -> "Or (" ^ formula_to_string f1 ^ ", " ^ formula_to_string f2 ^ ")"
  | Imply (f1, f2) -> "Imply (" ^ formula_to_string f1 ^ ", " ^ formula_to_string f2 ^ ")"
  | Iff (f1, f2) -> "Iff (" ^ formula_to_string f1 ^ ", " ^ formula_to_string f2 ^ ")") ^ ")"

(********************************************************************************************)
(************************************ TUNABLE PARAMETERS ************************************)
(********************************************************************************************)

(* @title A flag that decides whether the truth values (True & False) are included in a generated formula or not *)
let use_truth_value = true

(* @title An array that contains variable candidates
 * @dev The variables in this array can be included in a generated formula. *)
let var_src = [|"P"; "Q"; "R"|]

(* @title Parameters that limit the number of recursive calls of the formula generator function
 * @dev The formula generator function configures a formula recursively. The more it is called recursively,
 *      the longer formula it generates. And the following constraint must be kept.
 *      0 < min_recursion <= max_recursion *)
let min_recursion = 1
let max_recursion = 20

(********************************************************************************************)
(********************************************************************************************)
(********************************************************************************************)

(* @title A function that gets a random element from the given range of the given array
 * @param (a, b) the index range [a, b) *)
let random_element_range : int * int -> 'a Array.t -> 'a
=fun (a, b) arr -> Array.get arr (Random.int (b - a) + a)

(* @title A function that gets a random element from the given array *)
let random_element : 'a Array.t -> 'a
=fun arr -> random_element_range (0, Array.length arr) arr

(* @title A function that gets a random variable from the variable candidates *)
let random_var : unit -> formula
=fun () -> Var (random_element var_src)

(* @title A type to represent the grammatical items of propositional logic *)
type grammar_item = FALSE | TRUE | VAR | NOT | AND | OR | IMPLY | IFF
(* @title An array for the random formula generation *)
let formula_src = [|FALSE; TRUE; VAR; NOT; AND; OR; IMPLY; IFF|]

(* @title A function that configures a random formula recursively *)
let rec gen_formula ?(use_truth_value=use_truth_value) : int -> formula
=fun depth ->
  let choose =
    if depth = 1
      then if use_truth_value 
        then random_element [|random_element_range (0, 2); Fun.flip Array.get 2|]
        else Fun.flip Array.get 2
      else if max_recursion - depth < min_recursion 
          then random_element_range (3, 8) 
          else if use_truth_value 
            then random_element 
            else random_element_range (2, 8) in
  match choose formula_src with
  | FALSE -> False
  | TRUE -> True
  | VAR -> random_var()
  | NOT -> Not (gen_formula (depth - 1))
  | AND -> And (gen_formula (depth - 1), gen_formula (depth - 1))
  | OR -> Or (gen_formula (depth - 1), gen_formula (depth - 1))
  | IMPLY -> Imply (gen_formula (depth - 1), gen_formula (depth - 1))
  | IFF -> Iff (gen_formula (depth - 1), gen_formula (depth - 1))

(* @title A function that generates a random formula *)
let random_formula : unit -> formula
=fun () -> 
  if min_recursion > max_recursion || min_recursion <= 0 || max_recursion <= 0 
    then raise (Invalid_parameter "min_recursion, max_recursion") 
    else gen_formula max_recursion
