module System = struct
  type nat = Z | S of nat
  type op = Plus | Times
  type exp = Nat of nat | BinOp of op * exp * exp

  module Nat = struct
    type op = Plus | Times 
    type bin_op = BinOp of op * nat * nat
  end

  type judgement =
    | J_is of Nat.bin_op * nat
    | J_one_step of exp * exp
    | J_multi_step of exp * exp
    | J_det_step of exp * exp

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

  let result_of_judgement = function
    | J_is (_, n) -> Nat n
    | J_det_step (_, e) | J_one_step (_, e) | J_multi_step (_, e) -> e

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
      | Nat.BinOp (nat_op, n1, n2) ->
          of_nat n1 ^ " " ^ of_nat_op nat_op ^ " " ^ of_nat n2

    let of_judgement = function
      | J_is (nat_bin_op, n) -> of_nat_bin_op nat_bin_op ^ " is " ^ of_nat n
      | J_one_step (e1, e2) -> of_exp e1 ^ " ---> " ^ of_exp e2
      | J_multi_step (e1, e2) -> of_exp e1 ^ " -*-> " ^ of_exp e2
      | J_det_step (e1, e2) -> of_exp e1 ^ " -d-> " ^ of_exp e2

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
      let n = match n with Nat n -> n | _ -> assert false in
      Tree (J_is (nat_bin_op, S n), P_Succ, [t])
  | BinOp (Times, Z, _) -> Tree (J_is (nat_bin_op, Z), T_Zero, [])
  | BinOp (Times, S n1, n2) ->
      let t1 = derive_nat_bin_op (BinOp (Times, n1, n2)) in
      let n3 = result_of_judgement (root_of_tree t1) in
      let n3 = match n3 with Nat n3 -> n3 | _ -> assert false in
      let t2 = derive_nat_bin_op (BinOp (Plus, n2, n3)) in
      let n4 = result_of_judgement (root_of_tree t2) in
      let n4 = match n4 with Nat n4 -> n4 | _ -> assert false in
      Tree (J_is (nat_bin_op, n4), T_Succ, [t1; t2])

let rec derive_det_step exp =
  match exp with
  | Nat _ -> raise (Invalid_argument "Nat literal cannot be reduced.")
  | BinOp (Plus, Nat n1, Nat n2) ->
      let t = derive_nat_bin_op (Nat.BinOp (Nat.Plus, n1, n2)) in
      let n3 = result_of_judgement (root_of_tree t) in
      Tree (J_det_step (exp, n3), DR_Plus, [t])
  | BinOp (Times, Nat n1, Nat n2) ->
      let t = derive_nat_bin_op (Nat.BinOp (Nat.Times, n1, n2)) in
      let n3 = result_of_judgement (root_of_tree t) in
      Tree (J_det_step (exp, n3), DR_Times, [t])
  | BinOp (Plus, Nat n1, e2) ->
      let t = derive_det_step e2 in
      let e2' = result_of_judgement (root_of_tree t) in
      Tree (J_det_step (exp, BinOp (Plus, Nat n1, e2')), DR_PlusR, [t])
  | BinOp (Plus, e1, e2) ->
      let t = derive_det_step e1 in
      let e1' = result_of_judgement (root_of_tree t) in
      Tree (J_det_step (exp, BinOp (Plus, e1', e2)), DR_PlusL, [t])
  | BinOp (Times, Nat n1, e2) ->
      let t = derive_det_step e2 in
      let e2' = result_of_judgement (root_of_tree t) in
      Tree (J_det_step (exp, BinOp (Times, Nat n1, e2')), DR_TimesR, [t])
  | BinOp (Times, e1, e2) ->
      let t = derive_det_step e1 in
      let e1' = result_of_judgement (root_of_tree t) in
      Tree (J_det_step (exp, BinOp (Times, e1', e2)), DR_TimesL, [t])

let rec derive_one_step one_step =
  match one_step with
  | J_one_step (BinOp (Plus, Nat n1, Nat n2), n3) ->
      let t = derive_nat_bin_op (Nat.BinOp (Nat.Plus, n1, n2)) in
      let n3' = result_of_judgement (root_of_tree t) in
      assert (n3 = n3') ;
      Tree (one_step, R_Plus, [t])
  | J_one_step (BinOp (Times, Nat n1, Nat n2), n3) ->
      let t = derive_nat_bin_op (Nat.BinOp (Nat.Times, n1, n2)) in
      let n3' = result_of_judgement (root_of_tree t) in
      assert (n3 = n3') ;
      Tree (one_step, R_Times, [t])
  | J_one_step (BinOp (Plus, e1, e2), BinOp (Plus, e1', e2')) when e2 = e2' ->
      let t = derive_one_step (J_one_step (e1, e1')) in
      Tree (one_step, R_PlusL, [t])
  | J_one_step (BinOp (Plus, e1, e2), BinOp (Plus, e1', e2')) when e1 = e1' ->
      let t = derive_one_step (J_one_step (e2, e2')) in
      Tree (one_step, R_PlusR, [t])
  | J_one_step (BinOp (Times, e1, e2), BinOp (Plus, e1', e2')) when e2 = e2' ->
      let t = derive_one_step (J_one_step (e1, e1')) in
      Tree (one_step, R_TimesL, [t])
  | J_one_step (BinOp (Times, e1, e2), BinOp (Times, e1', e2')) when e1 = e1' ->
      let t = derive_one_step (J_one_step (e2, e2')) in
      Tree (one_step, R_TimesR, [t])
  | _ -> raise (Invalid_argument "Judgement is wrong.")

let rec eval_one_step exp =
  match exp with
  | BinOp (Plus, Nat n1, Nat n2) ->
      let t = derive_nat_bin_op (Nat.BinOp (Nat.Plus, n1, n2)) in
      result_of_judgement (root_of_tree t)
  | BinOp (Times, Nat n1, Nat n2) ->
      let t = derive_nat_bin_op (Nat.BinOp (Nat.Times, n1, n2)) in
      result_of_judgement (root_of_tree t)
  | BinOp ((_ as op), Nat n1, e2) -> BinOp (op, Nat n1, eval_one_step e2)
  | BinOp ((_ as op), e1, e2) -> BinOp (op, eval_one_step e1, e2)
  | _ -> raise (Invalid_argument "Nat cannot be reduced.")

let rec derive_multi_step multi_step =
  match multi_step with
  | J_multi_step (e, e'') -> (
      if e = e'' then Tree (multi_step, MR_Zero, [])
      else
        try
          let t = derive_one_step (J_one_step (e, e'')) in
          Tree (multi_step, MR_One, [t])
        with _ ->
          let e' = eval_one_step e in
          let t1 = derive_one_step (J_one_step (e, e')) in
          let t1 = Tree (J_multi_step (e, e'), MR_One, [t1]) in
          let t2 = derive_multi_step (J_multi_step (e', e'')) in
          Tree (multi_step, MR_Multi, [t1; t2]) )
  | _ -> raise (Invalid_argument "Judgement is wrong.")

let derive = function
  | J_is (nat_bin_op, _) -> derive_nat_bin_op nat_bin_op
  | J_one_step (_, _) as j -> derive_one_step j
  | J_multi_step (_, _) as j -> derive_multi_step j
  | J_det_step (e, _) -> derive_det_step e
