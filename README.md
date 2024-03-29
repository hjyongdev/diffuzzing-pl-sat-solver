# Random Input Differential Testing for Propositional Logic SAT Solver in OCaml
You can test your own SAT solver written in OCaml by using this project on a UNIX system.
## How it works
1. Generate grammatically correct propositional logic formulas randomly.
2. Input the generated formulas into both your own SAT solver and the Z3 solver.
3. If the two solvers answer in the same way, then PASS else FAIL.
4. Print the formulas that caused FAILs on the console.
## How to test
1. Implement your own SAT solver in **sat.ml**. Use the type `formula` in **input.ml**.
2. In the console,
    1. `dune build`
    2. `dune exec -- ./main.exe` or `./_build/default/main.exe`
  
By default, 100 random formulas are used in testing. But you can input the number of formulas for testing into the executable, e.g., `dune exec -- ./main.exe 1234`.
## Tunable parameters
There are some tunable parameters in **input.ml**.
- `use_truth_value`: A Boolean flag that decides whether the truth values (`True` & `False`) are included in a generated formula or not. If true, they are included.  

- `var_src`: An array that contains variable candidates. The variables in this array can be included in a generated formula.  

- `min_recursion` & `max_recursion`: Parameters that limit the number of recursive calls of the formula generator function. 
    - The formula generator function configures a formula recursively. The more it is called recursively, the longer formula it generates. And the following constraint must be kept.
    - 0 < min_recursion <= max_recursion
