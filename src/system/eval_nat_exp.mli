module Eval_nat_exp :
  sig
    type nat = Z | S of nat
    type nat_op = N_Plus | N_Times
    type nat_bin_op = N_bin_op of nat_op * nat * nat
    type op = Plus | Times
    type exp = Nat of nat | Exp of op * exp * exp
    type judgement = J_is of nat_bin_op * nat | J_evalto of exp * nat
    type rule =
        E_Const
      | E_Plus
      | E_Times
      | P_Zero
      | P_Succ
      | T_Zero
      | T_Succ
    module String :
      sig
        val of_nat : nat -> string
        val of_nat_op : nat_op -> string
        val of_nat_bin_op : nat_bin_op -> string
        val of_op : op -> string
        val of_exp : exp -> string
        val of_judgement : judgement -> string
        val of_rule : rule -> string
      end
    val string_of_judgement : judgement -> string
    val string_of_rule : rule -> string
  end
module Tree :
  sig
    type node = Eval_nat_exp.judgement
    type tree =
      Core.Derivation_tree.Make(Eval_nat_exp).tree =
        Tree of node * Eval_nat_exp.rule * tree list
    val root_of_tree : tree -> node
    val string_of_tree : tree -> string
  end
type judgement =
    J_is of Eval_nat_exp.nat_bin_op * Eval_nat_exp.nat
  | J_evalto of Eval_nat_exp.exp * Eval_nat_exp.nat
val derive_nat : Eval_nat_exp.nat_bin_op -> Tree.tree
val derive_exp : Eval_nat_exp.exp -> Tree.tree
