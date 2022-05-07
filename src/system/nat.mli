module Nat :
  sig
    type nat = Z | S of nat
    type op = Plus | Times
    type bin_op = Bin_op of op * nat * nat
    type judgement = J_is of bin_op * nat
    type rule = P_Zero | P_Succ | T_Zero | T_Succ
    module String :
      sig
        val of_nat : nat -> string
        val of_op : op -> string
        val of_bin_op : bin_op -> string
        val of_judgement : judgement -> string
        val of_rule : rule -> string
      end
    val string_of_judgement : judgement -> string
    val string_of_rule : rule -> string
  end
module Tree :
  sig
    type node = Nat.judgement
    type tree =
      Core.Derivation_tree.Make(Nat).tree =
        Tree of node * Nat.rule * tree list
    val root_of_tree : tree -> Nat.judgement
    val string_of_tree : tree -> string
  end
val derive : Nat.bin_op -> Tree.tree
