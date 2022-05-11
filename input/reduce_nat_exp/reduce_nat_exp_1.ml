open System.Reduce_nat_exp
open Reduce_nat_exp

let () =
  let e = Exp (Plus, Nat Z, Nat (S (S Z))) in
  let e' = Nat (S (S Z)) in
  let tree = derive_big_step (J_big_step(e, e')) in
  let s = Tree.string_of_tree tree in
  print_endline s
