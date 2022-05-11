module Eval_nat_exp = struct
  type nat = Nat.Nat.nat
  type op = Plus | Times
  type exp = Nat of nat | Exp of op * exp * exp
  type judgement = J_from_nat of Nat.Nat.judgement | J_evalto of exp * nat
  type rule = E_Const | E_Plus | E_Times 
  (* | P_Zero | P_Succ | T_Zero | T_Succ *)

  module String = struct
    let of_op = function Plus -> "+" | Times -> "*"

    let rec of_exp = function
      | Nat n -> Nat.Nat.String.of_nat n
      | Exp (Plus, e1, e2) ->
          let s2 =
            match e2 with
            | Exp (Plus, _, _) -> "(" ^ of_exp e2 ^ ")"
            | _ -> of_exp e2 in
          of_exp e1 ^ " " ^ of_op Plus ^ " " ^ s2
      | Exp (Times, e1, e2) ->
          let s1 =
            match e1 with
            | Exp (Plus, _, _) -> "(" ^ of_exp e1 ^ ")"
            | _ -> of_exp e1 in
          let s2 =
            match e2 with
            | Exp (_, _, _) -> "(" ^ of_exp e2 ^ ")"
            | _ -> of_exp e2 in
          s1 ^ " " ^ of_op Times ^ " " ^ s2

    let of_judgement = function
      | J_from_nat j -> Nat.Nat.string_of_judgement j
      | J_evalto (e, n) -> of_exp e ^ " evalto " ^ Nat.Nat.String.of_nat n

    let of_rule = function
      | E_Const -> "E-Const"
      | E_Plus -> "E-Plus"
      | E_Times -> "E-Times"
  end

  let string_of_judgement = String.of_judgement
  let string_of_rule = String.of_rule
end

module Tree = Core.Derivation_tree.Make (Eval_nat_exp)
open Eval_nat_exp
open Tree

let result_of_judgement = function J_is (_, n) -> n | J_evalto (_, n) -> n

type judgement = J_is of nat_bin_op * nat | J_evalto of exp * nat

let rec derive_exp exp =
  match exp with
  | Nat n -> Tree (J_evalto (exp, n), E_Const, [])
  | Exp (Plus, e1, e2) ->
      let t1 = derive_exp e1 in
      let n1 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_exp e2 in
      let n2 = result_of_judgement (root_of_tree t2) in
      let t3 = derive_nat (N_bin_op (N_Plus, n1, n2)) in
      let n3 = result_of_judgement (root_of_tree t3) in
      Tree (J_evalto (exp, n3), E_Plus, [t1; t2; t3])
  | Exp (Times, e1, e2) ->
      let t1 = derive_exp e1 in
      let n1 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_exp e2 in
      let n2 = result_of_judgement (root_of_tree t2) in
      let t3 = derive_nat (N_bin_op (N_Times, n1, n2)) in
      let n3 = result_of_judgement (root_of_tree t3) in
      Tree (J_evalto (exp, n3), E_Times, [t1; t2; t3])

let derive = function
| J_from_nat j -> Nat.derive j
| J_evalto(e, _) -> derive_exp e
