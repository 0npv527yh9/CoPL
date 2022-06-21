open System.Typing_ml4

let () =
  let judgement = Parser.toplevel Lexer.token (Lexing.from_channel stdin) in
  let tree = Tree.derive judgement in
  let s = Tree.string_of_tree tree in
  print_endline s
