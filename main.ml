open Input

exception Invalid_user_input of string

(* @title The default number of random formulas to be used in testing *)
let default_cnt = 100

(* @title A function that returns a random formula list
 * @param cnt the number of formulas to be generated *)
let gen_formulae : int -> formula list
=fun cnt ->
  Random.init(int_of_float (Unix.gettimeofday()));
  List.init cnt (fun _ -> Input.random_formula())

type result = int * int * int * (formula * bool * bool) list

(* @title A function that tests with the given formula list 
 * @return (the number of SAT formulas, 
 *          the number of UNSAT formulas,
 *          the number of fails,
 *          list of the formulas that caused fails) *)
let test : formula list -> result
=fun fl ->
  let line s = print_endline ("------------------- " ^ s ^ " -------------------\n") in
  let process_f : result -> formula -> result
  =fun (tc, fc, errc, err) f ->
    let user_res, z3_res = Sat.solve f, Smt.z3_solve f in
    print_endline ("formula = " ^ Input.formula_to_string f);
    print_endline ("You: " ^ string_of_bool user_res);
    print_endline ("Z3 : " ^ string_of_bool z3_res);
    if user_res = z3_res
      then (line "PASS"; (tc + (if user_res then 1 else 0), fc + (if user_res then 0 else 1), errc, err))
      else (line "FAIL"; (tc, fc, errc + 1, (f, user_res, z3_res)::err)) in
  List.fold_left process_f (0, 0, 0, []) fl

(* @title A function that prints the result of a test *)
let print_res : result -> int -> unit
=fun (tc, fc, errc, err) cnt ->
  let line, title = "************************************", "************** RESULT **************" in
  print_endline line; print_endline title; print_endline (line);
  print_endline ("# of SAT   formulas: " ^ string_of_int tc);
  print_endline ("# of UNSAT formulas: " ^ string_of_int fc);
  print_endline ("# of FAILs         : " ^ string_of_int errc);
  print_endline ("SCORE              : " ^ string_of_int (cnt - errc) ^ "/" ^ string_of_int cnt);
  if not (List.is_empty err) then 
    (print_endline "*********** FAILED CASES ***********";
    List.iter (fun (f, user_res, z3_res) ->
      print_endline ("formula = " ^ Input.formula_to_string f);
      print_endline ("You: " ^ string_of_bool user_res);
      print_endline ("Z3 : " ^ string_of_bool z3_res ^ "\n");) err)
  else print_endline line 

let _ = 
  let argc = Array.length Sys.argv in
  (* Input validation (1): the number of inputs *)
  if argc > 2 then raise (Invalid_user_input "Too many arguments") else
  let cnt = if argc = 1 then default_cnt 
            (* Input validation (2): the first user input is an integer or not *)  
            else try int_of_string Sys.argv.(1) with _ -> raise (Invalid_user_input "Must be an integer") in
  (* Input validation (3): the first user input is positive or not *)
  if cnt <= 0 then raise (Invalid_user_input "Must be positive") else
  let formulae = gen_formulae cnt in
  let res = test formulae in
  print_res res cnt
