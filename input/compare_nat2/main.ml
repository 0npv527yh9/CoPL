open System.Compare_nat2

let () =
  let judgement = Parser.toplevel Lexer.token (Lexing.from_channel stdin) in
  let tree = derive judgement in
  let s = Tree.string_of_tree tree in
  print_endline s
