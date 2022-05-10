module Nat = struct
  type nat = Z | S of nat
  type op = Plus | Times
  type bin_op = Bin_op of op * nat * nat
  type judgement = J_is of bin_op * nat
  type rule = P_Zero | P_Succ | T_Zero | T_Succ

  module String = struct
    let rec of_nat = function Z -> "Z" | S n -> "S(" ^ of_nat n ^ ")"
    let of_op = function Plus -> "plus" | Times -> "times"

    let of_bin_op = function
      | Bin_op (op, n1, n2) -> of_nat n1 ^ " " ^ of_op op ^ " " ^ of_nat n2

    let of_judgement = function
      | J_is (bin_op, n) -> of_bin_op bin_op ^ " is " ^ of_nat n

    let of_rule = function
      | P_Zero -> "P-Zero"
      | P_Succ -> "P-Succ"
      | T_Zero -> "T-Zero"
      | T_Succ -> "T-Succ"
  end

  let string_of_judgement = String.of_judgement
  let string_of_rule = String.of_rule
end

module Tree = Core.Derivation_tree.Make (Nat)
open Nat
open Tree

let result_of_judgement = function J_is (_, n) -> n

let rec derive_bin_op bin_op =
  match bin_op with
  | Bin_op (Plus, Z, n) -> Tree (J_is (bin_op, n), P_Zero, [])
  | Bin_op (Plus, S n1, n2) ->
      let t = derive_bin_op (Bin_op (Plus, n1, n2)) in
      let n = result_of_judgement (root_of_tree t) in
      Tree (J_is (bin_op, S n), P_Succ, [t])
  | Bin_op (Times, Z, _) -> Tree (J_is (bin_op, Z), T_Zero, [])
  | Bin_op (Times, S n1, n2) ->
      let t1 = derive_bin_op (Bin_op (Times, n1, n2)) in
      let n3 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_bin_op (Bin_op (Plus, n2, n3)) in
      let n4 = result_of_judgement (root_of_tree t2) in
      Tree (J_is (bin_op, n4), T_Succ, [t1; t2])

let derive = function J_is (bin_op, _) -> derive_bin_op bin_op
