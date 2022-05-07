open System.Compare_nat2
open Compare_nat2

let () =
  let judgement = J_less (S (S Z), S (S (S Z))) in
  let tree = derive judgement in
  let s = Tree.string_of_tree tree in
  print_endline s
