module Compare_nat2 :
  sig
    type nat = Z | S of nat
    type judgement = J_less of nat * nat
    type rule = L_Zero | L_SuccSucc
    module String :
      sig
        val of_nat : nat -> string
        val of_judgement : judgement -> string
        val of_rule : rule -> string
      end
    val string_of_judgement : judgement -> string
    val string_of_rule : rule -> string
  end
module Tree :
  sig
    type node = Compare_nat2.judgement
    type tree =
      Core.Derivation_tree.Make(Compare_nat2).tree =
        Tree of node * Compare_nat2.rule * tree list
    val root_of_tree : tree -> node
    val string_of_tree : tree -> string
  end
val derive : Tree.node -> Tree.tree
