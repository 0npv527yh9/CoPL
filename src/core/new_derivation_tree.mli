module Make :
  functor (System : New_system.System) ->
    sig
      type tree = Tree of System.judgement * System.rule * tree list
      val derive : System.judgement -> tree * System.subst
      val token_list_of_tree : tree -> Pprint.token list
      val token_list_of_tree_list :
        Pprint.token list -> tree list -> Pprint.token list
      val string_of_tree : tree -> string
    end
