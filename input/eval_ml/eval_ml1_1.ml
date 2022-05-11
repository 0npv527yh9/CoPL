open System.Eval_ml1
open Eval_ml1

let () =
  let e = Bin_op(Plus, ILit 3, ILit 5) in
  let tree = derive_evalto e in
  let s = Tree.string_of_tree tree in
  print_endline s
