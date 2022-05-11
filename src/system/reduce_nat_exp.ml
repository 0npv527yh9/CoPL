module Reduce_nat_exp = struct
  type nat = Z | S of nat
  type nat_op = N_Plus | N_Times
  type nat_bin_op = N_bin_op of nat_op * nat * nat
  type op = Plus | Times
  type exp = Nat of nat | Exp of op * exp * exp

  type judgement =
    | J_is of nat_bin_op * nat
    | J_small_step of exp * exp
    | J_big_step of exp * exp
    | J_reduce of exp * exp

  type rule =
    | R_Plus
    | R_Times
    | R_PlusL
    | R_PlusR
    | R_TimesL
    | R_TimesR
    | MR_Zero
    | MR_One
    | MR_Multi
    | DR_Plus
    | DR_Times
    | DR_PlusL
    | DR_PlusR
    | DR_TimesL
    | DR_TimesR
    | P_Zero
    | P_Succ
    | T_Zero
    | T_Succ

  module String = struct
    let rec of_nat = function Z -> "Z" | S n -> "S(" ^ of_nat n ^ ")"
    let of_nat_op = function N_Plus -> "plus" | N_Times -> "times"

    let of_nat_bin_op = function
      | N_bin_op (nat_op, n1, n2) ->
          of_nat n1 ^ " " ^ of_nat_op nat_op ^ " " ^ of_nat n2

    let of_op = function Plus -> "+" | Times -> "*"

    let rec of_exp = function
      | Nat n -> of_nat n
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
      | J_is (nat_bin_op, n) -> of_nat_bin_op nat_bin_op ^ " is " ^ of_nat n
      | J_small_step (e1, e2) -> of_exp e1 ^ " ---> " ^ of_exp e2
      | J_big_step (e1, e2) -> of_exp e1 ^ " -*-> " ^ of_exp e2
      | J_reduce (e1, e2) -> of_exp e1 ^ " -d-> " ^ of_exp e2

    let of_rule = function
      | R_Plus -> "R-Plus"
      | R_Times -> "R-Times"
      | R_PlusL -> "R-PlusL"
      | R_PlusR -> "R-PlusR"
      | R_TimesL -> "R-TimesL"
      | R_TimesR -> "R-TimesR"
      | MR_Zero -> "MR-Zero"
      | MR_One -> "MR-One"
      | MR_Multi -> "MR-Multi"
      | DR_Plus -> "DR-Plus"
      | DR_Times -> "DR-Times"
      | DR_PlusL -> "DR-PlusL"
      | DR_PlusR -> "DR-PlusR"
      | DR_TimesL -> "DR-TimesL"
      | DR_TimesR -> "DR-TimesR"
      | P_Zero -> "P-Zero"
      | P_Succ -> "P-Succ"
      | T_Zero -> "T-Zero"
      | T_Succ -> "T-Succ"
  end

  let string_of_judgement = String.of_judgement
  let string_of_rule = String.of_rule
end

module Tree = Core.Derivation_tree.Make (Reduce_nat_exp)
open Reduce_nat_exp
open Tree

let result_of_J_is = function J_is (_, n) -> n | _ -> Z
let result_of_J_reduce = function J_reduce (_, e) -> e | _ -> Nat Z
let result_of_J_small_step = function J_small_step (_, e) -> e | _ -> Nat Z
let result_of_J_big_step = function J_big_step (_, e) -> e | _ -> Nat Z

let rec derive_nat nat_bin_op =
  match nat_bin_op with
  | N_bin_op (N_Plus, Z, n) -> Tree (J_is (nat_bin_op, n), P_Zero, [])
  | N_bin_op (N_Plus, S n1, n2) ->
      let t = derive_nat (N_bin_op (N_Plus, n1, n2)) in
      let n = result_of_J_is (root_of_tree t) in
      Tree (J_is (nat_bin_op, S n), P_Succ, [t])
  | N_bin_op (N_Times, Z, _) -> Tree (J_is (nat_bin_op, Z), T_Zero, [])
  | N_bin_op (N_Times, S n1, n2) ->
      let t1 = derive_nat (N_bin_op (N_Times, n1, n2)) in
      let n3 = result_of_J_is (root_of_tree t1) in
      let t2 = derive_nat (N_bin_op (N_Plus, n2, n3)) in
      let n4 = result_of_J_is (root_of_tree t2) in
      Tree (J_is (nat_bin_op, n4), T_Succ, [t1; t2])

let rec derive_exp exp =
  match exp with
  | Nat _ -> raise (Invalid_argument "Nat literal cannot be reduced.")
  | Exp (Plus, Nat n1, Nat n2) ->
      let t = derive_nat (N_bin_op (N_Plus, n1, n2)) in
      let n3 = result_of_J_is (root_of_tree t) in
      Tree (J_reduce (exp, Nat n3), DR_Plus, [t])
  | Exp (Times, Nat n1, Nat n2) ->
      let t = derive_nat (N_bin_op (N_Times, n1, n2)) in
      let n3 = result_of_J_is (root_of_tree t) in
      Tree (J_reduce (exp, Nat n3), DR_Times, [t])
  | Exp (Plus, Nat n1, e2) ->
      let t = derive_exp e2 in
      let e2' = result_of_J_small_step (root_of_tree t) in
      Tree (J_reduce (exp, Exp (Plus, Nat n1, e2')), DR_PlusR, [t])
  | Exp (Plus, e1, e2) ->
      let t = derive_exp e1 in
      let e1' = result_of_J_small_step (root_of_tree t) in
      Tree (J_reduce (exp, Exp (Plus, e1', e2)), DR_PlusL, [t])
  | Exp (Times, Nat n1, e2) ->
      let t = derive_exp e2 in
      let e2' = result_of_J_small_step (root_of_tree t) in
      Tree (J_reduce (exp, Exp (Times, Nat n1, e2')), DR_TimesR, [t])
  | Exp (Times, e1, e2) ->
      let t = derive_exp e1 in
      let e1' = result_of_J_small_step (root_of_tree t) in
      Tree (J_reduce (exp, Exp (Times, e1', e2)), DR_TimesL, [t])

let rec derive_small_step exp =
  match exp with
  | Nat _ -> raise (Invalid_argument "Nat literal cannot be reduced.")
  | Exp (Plus, Nat n1, Nat n2) ->
      let t = derive_nat (N_bin_op (N_Plus, n1, n2)) in
      let n3 = result_of_J_is (root_of_tree t) in
      Tree (J_small_step (exp, Nat n3), R_Plus, [t])
  | Exp (Times, Nat n1, Nat n2) ->
      let t = derive_nat (N_bin_op (N_Times, n1, n2)) in
      let n3 = result_of_J_is (root_of_tree t) in
      Tree (J_small_step (exp, Nat n3), R_Times, [t])
  | Exp (Plus, Nat n1, e2) ->
      let t = derive_small_step e2 in
      let e2' = result_of_J_small_step (root_of_tree t) in
      Tree (J_small_step (exp, Exp (Plus, Nat n1, e2')), R_PlusR, [t])
  | Exp (Plus, e1, e2) ->
      let t = derive_small_step e1 in
      let e1' = result_of_J_small_step (root_of_tree t) in
      Tree (J_small_step (exp, Exp (Plus, e1', e2)), R_PlusL, [t])
  | Exp (Times, Nat n1, e2) ->
      let t = derive_small_step e2 in
      let e2' = result_of_J_small_step (root_of_tree t) in
      Tree (J_small_step (exp, Exp (Times, Nat n1, e2')), R_TimesR, [t])
  | Exp (Times, e1, e2) ->
      let t = derive_small_step e1 in
      let e1' = result_of_J_small_step (root_of_tree t) in
      Tree (J_small_step (exp, Exp (Times, e1', e2)), R_TimesL, [t])

let rec derive_big_step big_step =
  match big_step with
  | J_big_step (e, e'') -> (
    try
      let t = derive_small_step e in
      let judgement = root_of_tree t in
      match judgement with
      | J_small_step (_, e') ->
          if e' = e'' then Tree (J_big_step (e, e'), MR_One, [t])
          else
            let t1 = Tree (J_big_step (e, e'), MR_One, [t]) in
            let t2 = derive_big_step (J_big_step (e', e'')) in
            Tree (J_big_step (e, e''), MR_Multi, [t1; t2])
      | _ -> raise (Invalid_argument "a")
    with _ -> Tree (J_big_step (e, e), MR_Zero, []) )
  | _ -> raise (Invalid_argument "a")
