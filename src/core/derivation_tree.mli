module Make :
  functor (System : System.System) ->
    sig
      type node = System.judgement
      type tree = Tree of node * System.rule * tree list
      val root_of_tree : tree -> node
      val string_of_tree : tree -> string
    end
