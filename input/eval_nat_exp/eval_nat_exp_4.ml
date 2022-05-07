open System.Eval_nat_exp
open Eval_nat_exp

let () =
  let e = Exp (Times, Nat (S (S Z)), Nat (S Z)) in
  let e = Exp (Plus, Nat (S (S (S Z))), e) in
  let tree = derive_exp e in
  let s = Tree.string_of_tree tree in
  print_endline s
