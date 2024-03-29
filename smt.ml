open Input

exception Z3_unknown
exception Z3_failed

(* @title A module for Z3 context *)
module Ctx = struct
  type t = Z3.context

  let ctx : t option ref = ref None

  let init : unit -> unit
  =fun () -> ctx := Some (Z3.mk_context [])

  let get : unit -> t
  =fun () -> let _ = if Option.is_none !ctx then init() in Option.get !ctx
end

(* @title A function that translates a formula to a Z3 Boolean expression *)
let rec trans : formula -> Z3.Expr.expr
=fun f -> match f with
  | False -> Z3.Boolean.mk_false (Ctx.get())
  | True -> Z3.Boolean.mk_true (Ctx.get())
  | Var x -> Z3.Boolean.mk_const_s (Ctx.get()) x
  | Not f -> Z3.Boolean.mk_not (Ctx.get()) (trans f)
  | And (f1, f2) -> Z3.Boolean.mk_and (Ctx.get()) [trans f1; trans f2]
  | Or (f1, f2) -> Z3.Boolean.mk_or (Ctx.get()) [trans f1; trans f2]
  | Imply (f1, f2) -> Z3.Boolean.mk_implies (Ctx.get()) (trans f1) (trans f2)
  | Iff (f1, f2) -> Z3.Boolean.mk_iff (Ctx.get()) (trans f1) (trans f2)

(* @title A function that decides if the given formula is satisfiable or not by using Z3 solver *)
let z3_solve : formula -> bool
=fun f ->
  let solver = Z3.Solver.mk_solver (Ctx.get()) None in
  match Z3.Solver.check solver [trans f] with
  | UNKNOWN -> raise Z3_unknown
  | UNSATISFIABLE -> false
  | SATISFIABLE ->
    if Option.is_none (Z3.Solver.get_model solver)
      then raise Z3_failed
      else true
