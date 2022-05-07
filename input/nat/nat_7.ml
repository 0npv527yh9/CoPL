open System.Nat
open Nat

let () =
  let bin_op = Bin_op (Times, S (S Z), S Z) in
  let tree = derive bin_op in
  let s = Tree.string_of_tree tree in
  print_endline s
