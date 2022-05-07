open System.Nat
open Nat

let () =
  let bin_op = Bin_op (Plus, S (S Z), Z) in
  let tree = derive bin_op in
  let s = Tree.string_of_tree tree in
  print_endline s
