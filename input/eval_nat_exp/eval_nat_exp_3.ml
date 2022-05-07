open System.Eval_nat_exp
open Eval_nat_exp

let () =
  let e = Exp (Plus, Nat (S Z), Nat (S Z)) in
  let e = Exp (Plus, e, Nat (S Z)) in
  let tree = derive_exp e in
  let s = Tree.string_of_tree tree in
  print_endline s
