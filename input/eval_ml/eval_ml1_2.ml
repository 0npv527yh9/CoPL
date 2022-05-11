open System.Eval_ml1
open Eval_ml1

let () =
  let e1 = Bin_op(Minus, ILit 8, ILit 2) in
  let e = Bin_op(Minus, e1, ILit 3) in
  let tree = derive_evalto e in
  let s = Tree.string_of_tree tree in
  print_endline s
