open System.Reduce_nat_exp
open Reduce_nat_exp

let () =
  let e1 = Exp (Times, Nat (S Z), Nat (S Z)) in
  let e2 = Exp (Times, Nat (S Z), Nat (S Z)) in
  let e = Exp (Plus, e1, e2) in
  let tree = derive_exp e in
  let s = Tree.string_of_tree tree in
  print_endline s
