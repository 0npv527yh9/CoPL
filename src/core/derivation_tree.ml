open Judgement

module type Tree = sig
  type node
  type tree

  val root_of_tree : tree -> node
  val string_of_tree : tree -> string
end

module Make (Judgement : Judgement) : Tree = struct
  open Pprint
  open Judgement

  type node = judgement
  type tree = Tree of judgement * rule * tree list

  let root_of_tree = function Tree (node, _, _) -> node

  let rec token_list_of_tree = function
    | Tree (judgement, rule, subtree_list) ->
        let head =
          Line
            (string_of_judgement judgement ^ " by " ^ string_of_rule rule ^ " {")
        in
        let body = token_list_of_tree_list [] subtree_list in
        let tail = Line "}" in
        (head :: Indent :: body) @ [Unindent; tail]

  and token_list_of_tree_list token_list = function
    | [] -> List.rev_append token_list []
    | [subtree] -> List.rev_append token_list (token_list_of_tree subtree)
    | subtree :: subtree_list ->
        token_list_of_tree_list
          (Line ";" :: List.rev_append (token_list_of_tree subtree) token_list)
          subtree_list

  let string_of_tree tree = string_of_token_list (token_list_of_tree tree)
end
