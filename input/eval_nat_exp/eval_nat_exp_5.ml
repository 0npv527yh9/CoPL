open System.Eval_nat_exp
open Eval_nat_exp

let () =
  let e = Exp (Plus, Nat (S (S Z)), Nat (S (S Z))) in
  let e = Exp (Times, e, Nat Z) in
  let tree = derive_exp e in
  let s = Tree.string_of_tree tree in
  print_endline s
