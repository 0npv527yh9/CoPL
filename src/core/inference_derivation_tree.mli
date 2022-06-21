module Make :
  functor (System : Inference_system.System) ->
    sig
      type tree = Tree of System.judgement * System.rule * tree list
      val derive : System.judgement -> tree
      val string_of_tree : tree -> string
    end
