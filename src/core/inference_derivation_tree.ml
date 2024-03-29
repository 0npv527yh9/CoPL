open Inference_system

module Make (System : System) = struct
  open Pprint
  open System

  type tree = Tree of judgement * rule * tree list

  let subst_judgement subst judgement =
    List.fold_left
      (fun judgement one_subst -> one_subst_judgement one_subst judgement)
      judgement subst

  let rec subst_tree subst = function
    | Tree (judgement, rule, sub_tree_list) ->
        Tree
          ( subst_judgement subst judgement
          , rule
          , List.map (subst_tree subst) sub_tree_list )

  let rec derive judgement =
    let rule, sub_judgement_list, eqs = inversion judgement in
    let rec rec_derive = function
      | [] -> ([], [])
      | sub_judgement :: sub_judgement_list -> (
          let tree, subst = derive sub_judgement in
          match rec_derive sub_judgement_list with
          | tree_list, subst' ->
              ( tree :: tree_list
              , unify (eqs_of_subst subst @ eqs_of_subst subst') ) ) in
    let sub_tree_list, subst = rec_derive sub_judgement_list in
    let eqs = eqs @ eqs_of_subst subst in
    let subst = unify eqs in
    (Tree (judgement, rule, sub_tree_list), subst)

  let derive judgement =
    match derive judgement with tree, subst -> subst_tree subst tree

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
