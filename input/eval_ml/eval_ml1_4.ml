open System.Eval_ml1
open Eval_ml1

let () =
  let e1 = Bin_op(Lt, ILit 4, ILit 5) in
  let e2 = Bin_op(Plus, ILit 2, ILit 3) in
  let e3 = Bin_op(Times, ILit 8, ILit 8) in
  let e = IfExp(e1, e2, e3) in
  let tree = derive_evalto e in
  let s = Tree.string_of_tree tree in
  print_endline s
