open System.Eval_nat_exp
open Eval_nat_exp

let () =
  let exp = Exp (Plus, Nat (S (S Z)), Nat (S (S Z))) in
  let tree = derive_exp exp in
  let s = Tree.string_of_tree tree in
  print_endline s
