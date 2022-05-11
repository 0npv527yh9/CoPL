module Eval_ml1 = struct
  type value = IntV of int | BoolV of bool
  type value_op = V_Plus | V_Minus | V_Times | V_Lt
  type value_bin_op = V_bin_op of value_op * value * value
  type op = Plus | Minus | Times | Lt

  type exp =
    | ILit of int
    | BLit of bool
    | Bin_op of op * exp * exp
    | IfExp of exp * exp * exp

  type judgement = J_evalto of exp * value | J_is of value_bin_op * value

  type rule =
    | E_Int
    | E_Bool
    | E_IfT
    | E_IfF
    | E_Plus
    | E_Minus
    | E_Times
    | E_Lt
    | B_Plus
    | B_Minus
    | B_Times
    | B_Lt

  module String = struct
    let of_value = function
      | IntV i -> string_of_int i
      | BoolV b -> string_of_bool b

    let of_value_op = function
      | V_Plus -> "plus"
      | V_Minus -> "minus"
      | V_Times -> "times"
      | V_Lt -> "less than"

    let of_value_bin_op = function
      | V_bin_op (value_op, v1, v2) ->
          of_value v1 ^ " " ^ of_value_op value_op ^ " " ^ of_value v2

    let of_op = function Plus -> "+" | Minus -> "-" | Times -> "*" | Lt -> "<"

    let rec of_exp = function
      | ILit i -> string_of_int i
      | BLit b -> string_of_bool b
      | Bin_op ((Plus | Minus) as op, e1, e2) ->
          let s2 =
            match e2 with
            | Bin_op ((Plus | Minus) , _, _) -> "(" ^ of_exp e2 ^ ")"
            | _ -> of_exp e2 in
          of_exp e1 ^ " " ^ of_op op ^ " " ^ s2
      | Bin_op (Times, e1, e2) ->
          let s1 =
            match e1 with
            | Bin_op ((Plus | Minus), _, _) -> "(" ^ of_exp e1 ^ ")"
            | _ -> of_exp e1 in
          let s2 =
            match e2 with
            | Bin_op (_, _, _) -> "(" ^ of_exp e2 ^ ")"
            | _ -> of_exp e2 in
          s1 ^ " " ^ of_op Times ^ " " ^ s2
      | Bin_op (Lt, e1, e2) -> of_exp e1 ^ " " ^ of_op Lt ^ " " ^ of_exp e2
      | IfExp (e1, e2, e3) ->
          "if " ^ of_exp e1 ^ " then " ^ of_exp e2 ^ " else " ^ of_exp e3

    let of_judgement = function
      | J_evalto (exp, value) -> of_exp exp ^ " evalto " ^ of_value value
      | J_is (value_bin_op, value) ->
          of_value_bin_op value_bin_op ^ " is " ^ of_value value

    let of_rule = function
      | E_Int -> "E-Int"
      | E_Bool -> "E-Bool"
      | E_IfT -> "E-IfT"
      | E_IfF -> "E-IfF"
      | E_Plus -> "E-Plus"
      | E_Minus -> "E-Minus"
      | E_Times -> "E-Times"
      | E_Lt -> "E-Lt"
      | B_Plus -> "B-Plus"
      | B_Minus -> "B-Minus"
      | B_Times -> "B-Times"
      | B_Lt -> "B-Lt"
  end

  let string_of_judgement = String.of_judgement
  let string_of_rule = String.of_rule
end

module Tree = Core.Derivation_tree.Make (Eval_ml1)
open Eval_ml1
open Tree

let result_of_judgement = function J_evalto (_, v) -> v | J_is (_, v) -> v

let derive_is value_bin_op =
  match value_bin_op with
  | V_bin_op (V_Plus, IntV i1, IntV i2) ->
      Tree (J_is (value_bin_op, IntV (i1 + i2)), B_Plus, [])
  | V_bin_op (V_Minus, IntV i1, IntV i2) ->
      Tree (J_is (value_bin_op, IntV (i1 - i2)), B_Minus, [])
  | V_bin_op (V_Times, IntV i1, IntV i2) ->
      Tree (J_is (value_bin_op, IntV (i1 * i2)), B_Times, [])
  | V_bin_op (V_Lt, IntV i1, IntV i2) ->
      Tree (J_is (value_bin_op, BoolV (i1 < i2)), B_Lt, [])
  | _ ->
      raise
        (Invalid_argument
           (String.of_value_bin_op value_bin_op ^ " cannot be derived.") )

let rec derive_evalto exp =
  match exp with
  | ILit i -> Tree (J_evalto (exp, IntV i), E_Int, [])
  | BLit b -> Tree (J_evalto (exp, BoolV b), E_Bool, [])
  | IfExp (e1, e2, e3) -> (
      let t1 = derive_evalto e1 in
      let v1 = result_of_judgement (root_of_tree t1) in
      match v1 with
      | BoolV true ->
          let t2 = derive_evalto e2 in
          let v = result_of_judgement (root_of_tree t2) in
          Tree (J_evalto (exp, v), E_IfT, [t1; t2])
      | BoolV false ->
          let t2 = derive_evalto e3 in
          let v = result_of_judgement (root_of_tree t2) in
          Tree (J_evalto (exp, v), E_IfT, [t1; t2])
      | _ -> raise (Invalid_argument "v1 must be bool") )
  | Bin_op (Plus, e1, e2) ->
      let t1 = derive_evalto e1 in
      let i1 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_evalto e2 in
      let i2 = result_of_judgement (root_of_tree t2) in
      let t3 = derive_is (V_bin_op (V_Plus, i1, i2)) in
      let i3 = result_of_judgement (root_of_tree t3) in
      Tree (J_evalto (exp, i3), E_Plus, [t1; t2; t3])
  | Bin_op (Minus, e1, e2) ->
      let t1 = derive_evalto e1 in
      let i1 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_evalto e2 in
      let i2 = result_of_judgement (root_of_tree t2) in
      let t3 = derive_is (V_bin_op (V_Minus, i1, i2)) in
      let i3 = result_of_judgement (root_of_tree t3) in
      Tree (J_evalto (exp, i3), E_Minus, [t1; t2; t3])
  | Bin_op (Times, e1, e2) ->
      let t1 = derive_evalto e1 in
      let i1 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_evalto e2 in
      let i2 = result_of_judgement (root_of_tree t2) in
      let t3 = derive_is (V_bin_op (V_Times, i1, i2)) in
      let i3 = result_of_judgement (root_of_tree t3) in
      Tree (J_evalto (exp, i3), E_Times, [t1; t2; t3])
  | Bin_op (Lt, e1, e2) ->
      let t1 = derive_evalto e1 in
      let i1 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_evalto e2 in
      let i2 = result_of_judgement (root_of_tree t2) in
      let t3 = derive_is (V_bin_op (V_Lt, i1, i2)) in
      let i3 = result_of_judgement (root_of_tree t3) in
      Tree (J_evalto (exp, i3), E_Lt, [t1; t2; t3])
