open System.Eval_ml1
open Eval_ml1

let () =
  let e1 = Bin_op(Plus, ILit 4, ILit 5) in
  let e2 = Bin_op(Minus, ILit 1, ILit 10) in
  let e = Bin_op(Times, e1, e2) in
  let tree = derive_evalto e in
  let s = Tree.string_of_tree tree in
  print_endline s
