module System = struct
  type nat = Z | S of nat
  type op = Plus | Times
  type exp = Nat of nat | BinOp of op * exp * exp

  module Nat = struct
    type op = Plus | Times 
    type bin_op = BinOp of op * nat * nat
  end

  type judgement = J_evalto of exp * nat | J_is of Nat.bin_op * nat
  type rule = E_Const | E_Plus | E_Times | P_Zero | P_Succ | T_Zero | T_Succ

  let result_of_judgement = function J_is (_, n) | J_evalto (_, n) -> n

  module String = struct
    let rec of_nat = function Z -> "Z" | S n -> "S(" ^ of_nat n ^ ")"
    let of_op = function Plus -> "+" | Times -> "*"

    let rec of_exp = function
      | Nat n -> of_nat n
      | BinOp (Plus, e1, e2) ->
          let s2 =
            match e2 with
            | BinOp (Plus, _, _) -> "(" ^ of_exp e2 ^ ")"
            | _ -> of_exp e2 in
          of_exp e1 ^ " " ^ of_op Plus ^ " " ^ s2
      | BinOp (Times, e1, e2) ->
          let s1 =
            match e1 with
            | BinOp (Plus, _, _) -> "(" ^ of_exp e1 ^ ")"
            | _ -> of_exp e1 in
          let s2 =
            match e2 with
            | BinOp (_, _, _) -> "(" ^ of_exp e2 ^ ")"
            | _ -> of_exp e2 in
          s1 ^ " " ^ of_op Times ^ " " ^ s2

    let of_nat_op = function Nat.Plus -> "plus" | Nat.Times -> "times"

    let of_nat_bin_op = function
      | Nat.BinOp (op, n1, n2) ->
          of_nat n1 ^ " " ^ of_nat_op op ^ " " ^ of_nat n2

    let of_judgement = function
      | J_evalto (e, n) -> of_exp e ^ " evalto " ^ of_nat n
      | J_is (bin_op, n) -> of_nat_bin_op bin_op ^ " is " ^ of_nat n

    let of_rule = function
      | E_Const -> "E-Const"
      | E_Plus -> "E-Plus"
      | E_Times -> "E-Times"
      | P_Zero -> "P-Zero"
      | P_Succ -> "P-Succ"
      | T_Zero -> "T-Zero"
      | T_Succ -> "T-Succ"
  end

  let string_of_judgement = String.of_judgement
  let string_of_rule = String.of_rule
end

module Tree = Core.Derivation_tree.Make (System)
open System
open Tree

let rec derive_nat_bin_op nat_bin_op =
  let open Nat in
  match nat_bin_op with
  | BinOp (Plus, Z, n) -> Tree (J_is (nat_bin_op, n), P_Zero, [])
  | BinOp (Plus, S n1, n2) ->
      let t = derive_nat_bin_op (BinOp (Plus, n1, n2)) in
      let n = result_of_judgement (root_of_tree t) in
      Tree (J_is (nat_bin_op, S n), P_Succ, [t])
  | BinOp (Times, Z, _) -> Tree (J_is (nat_bin_op, Z), T_Zero, [])
  | BinOp (Times, S n1, n2) ->
      let t1 = derive_nat_bin_op (BinOp (Times, n1, n2)) in
      let n3 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_nat_bin_op (BinOp (Plus, n2, n3)) in
      let n4 = result_of_judgement (root_of_tree t2) in
      Tree (J_is (nat_bin_op, n4), T_Succ, [t1; t2])

let rec derive_exp exp =
  match exp with
  | Nat n -> Tree (J_evalto (exp, n), E_Const, [])
  | BinOp (Plus, e1, e2) ->
      let t1 = derive_exp e1 in
      let n1 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_exp e2 in
      let n2 = result_of_judgement (root_of_tree t2) in
      let t3 = derive_nat_bin_op (BinOp (Plus, n1, n2)) in
      let n3 = result_of_judgement (root_of_tree t3) in
      Tree (J_evalto (exp, n3), E_Plus, [t1; t2; t3])
  | BinOp (Times, e1, e2) ->
      let t1 = derive_exp e1 in
      let n1 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_exp e2 in
      let n2 = result_of_judgement (root_of_tree t2) in
      let t3 = derive_nat_bin_op (BinOp (Times, n1, n2)) in
      let n3 = result_of_judgement (root_of_tree t3) in
      Tree (J_evalto (exp, n3), E_Times, [t1; t2; t3])

let derive = function
  | J_evalto (e, _) -> derive_exp e
  | J_is (nat_bin_op, _) -> derive_nat_bin_op nat_bin_op
