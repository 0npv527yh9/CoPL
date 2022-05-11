open System.Eval_ml1
open Eval_ml1

let () =
  let e1 = ILit 3 in
  let e2 = Bin_op(Times, ILit (-2), ILit 8) in
  let e2 = Bin_op(Lt, ILit (-23), e2) in
  let e3 = ILit 8 in
  let e4 = ILit 2 in
  let e2 = IfExp(e2, e3, e4) in
  let e1 = Bin_op(Plus, e1, e2) in
  let e = Bin_op(Plus, e1, ILit 4) in
  let tree = derive_evalto e in
  let s = Tree.string_of_tree tree in
  print_endline s
