module System = struct
  type op = Plus | Minus | Times | Lt
  type var = string

  type exp =
    | E_int of int
    | E_bool of bool
    | E_var of var
    | BinOp of op * exp * exp
    | E_if of exp * exp * exp
    | E_let of var * exp * exp
    | E_fun of var * exp
    | E_app of exp * exp
    | E_letrec of var * var * exp * exp

  type value =
    | V_int of int
    | V_bool of bool
    | V_fun of env * var * exp
    | V_letrec of env * var * var * exp

  and env = (var * value) list

  type judgement =
    | J_evalto of env * exp * value
    | J_is of (op * value * value) * value

  type rule =
    | E_Int
    | E_Bool
    | E_IfT
    | E_IfF
    | E_Plus
    | E_Minus
    | E_Times
    | E_Lt
    | E_Var1
    | E_Var2
    | E_Let
    | E_Fun
    | E_App
    | E_LetRec
    | E_AppRec
    | B_Plus
    | B_Minus
    | B_Times
    | B_Lt

  let result_of_judgement = function J_evalto (_, _, v) | J_is (_, v) -> v

  let rule_of_op = function
    | Plus -> E_Plus
    | Minus -> E_Minus
    | Times -> E_Times
    | Lt -> E_Lt

  module String = struct
    let of_op = function Plus -> "+" | Minus -> "-" | Times -> "*" | Lt -> "<"

    let of_meta_op = function
      | Plus -> "plus"
      | Minus -> "minus"
      | Times -> "times"
      | Lt -> "less than"

    let rec of_exp = function
      | E_int i -> string_of_int i
      | E_bool b -> string_of_bool b
      | E_var x -> x
      | BinOp (((Plus | Minus) as op), e1, e2) ->
          let s1 =
            match e1 with E_if _ -> "(" ^ of_exp e1 ^ ")" | _ -> of_exp e1 in
          let s2 =
            match e2 with
            | BinOp ((Plus | Minus), _, _) | E_if _ -> "(" ^ of_exp e2 ^ ")"
            | _ -> of_exp e2 in
          s1 ^ " " ^ of_op op ^ " " ^ s2
      | BinOp (Times, e1, e2) ->
          let s1 =
            match e1 with
            | BinOp ((Plus | Minus), _, _) | E_if _ -> "(" ^ of_exp e1 ^ ")"
            | _ -> of_exp e1 in
          let s2 =
            match e2 with
            | BinOp (_, _, _) | E_if _ -> "(" ^ of_exp e2 ^ ")"
            | _ -> of_exp e2 in
          s1 ^ " " ^ of_op Times ^ " " ^ s2
      | BinOp (Lt, e1, e2) ->
          let s1 =
            match e1 with E_if _ -> "(" ^ of_exp e1 ^ ")" | _ -> of_exp e1 in
          let s2 =
            match e2 with E_if _ -> "(" ^ of_exp e2 ^ ")" | _ -> of_exp e2 in
          s1 ^ " " ^ of_op Lt ^ " " ^ s2
      | E_if (e1, e2, e3) ->
          "if " ^ of_exp e1 ^ " then " ^ of_exp e2 ^ " else " ^ of_exp e3
      | E_let (x, e1, e2) -> "let " ^ x ^ " = " ^ of_exp e1 ^ " in " ^ of_exp e2
      | E_fun (x, e) -> "fun " ^ x ^ " -> " ^ of_exp e
      | E_app (e1, e2) ->
          let wrap = function
            | (E_var _ | E_int _ | E_bool _) as e -> of_exp e
            | e -> "(" ^ of_exp e ^ ")" in
          wrap e1 ^ " " ^ wrap e2
      | E_letrec (f, x, e1, e2) ->
          "let rec " ^ f ^ " = fun " ^ x ^ " -> " ^ of_exp e1 ^ " in "
          ^ of_exp e2

    let rec of_value = function
      | V_int i -> string_of_int i
      | V_bool b -> string_of_bool b
      | V_fun (env, x, e) ->
          "(" ^ of_env env ^ ")[fun " ^ x ^ " -> " ^ of_exp e ^ "]"
      | V_letrec (env, f, x, e) ->
          "(" ^ of_env env ^ ")[rec " ^ f ^ " = fun " ^ x ^ " -> " ^ of_exp e
          ^ "]"

    and of_env = function
      | [] -> ""
      | [(x, v)] -> x ^ " = " ^ of_value v
      | (x, v) :: env -> of_env env ^ ", " ^ x ^ " = " ^ of_value v

    let of_judgement = function
      | J_evalto (env, e, v) ->
          of_env env ^ " |- " ^ of_exp e ^ " evalto " ^ of_value v
      | J_is ((op, v1, v2), v3) ->
          of_value v1 ^ " " ^ of_meta_op op ^ " " ^ of_value v2 ^ " is "
          ^ of_value v3

    let of_rule = function
      | E_Int -> "E-Int"
      | E_Bool -> "E-Bool"
      | E_IfT -> "E-IfT"
      | E_IfF -> "E-IfF"
      | E_Plus -> "E-Plus"
      | E_Minus -> "E-Minus"
      | E_Times -> "E-Times"
      | E_Lt -> "E-Lt"
      | E_Var1 -> "E-Var1"
      | E_Var2 -> "E-Var2"
      | E_Let -> "E-Let"
      | E_Fun -> "E-Fun"
      | E_App -> "E-App"
      | E_LetRec -> "E-LetRec"
      | E_AppRec -> "E-AppRec"
      | B_Plus -> "B-Plus"
      | B_Minus -> "B-Minus"
      | B_Times -> "B-Times"
      | B_Lt -> "B-Lt"
  end

  let string_of_judgement = String.of_judgement
  let string_of_rule = String.of_rule
end

module Tree = Core.Derivation_tree.Make (System)
open System
open Tree

let rec derive_exp e env =
  match e with
  | E_int i -> Tree (J_evalto (env, e, V_int i), E_Int, [])
  | E_bool b -> Tree (J_evalto (env, e, V_bool b), E_Bool, [])
  | E_var x -> (
    match env with
    | (x', v) :: _ when x' = x -> Tree (J_evalto (env, e, v), E_Var1, [])
    | (y, _) :: env' when y != x ->
        let t = derive_exp e env' in
        let v = result_of_judgement (root_of_tree t) in
        Tree (J_evalto (env, e, v), E_Var2, [t])
    | _ -> assert false )
  | E_if (e1, e2, e3) -> (
      let t1 = derive_exp e1 env in
      let v1 = result_of_judgement (root_of_tree t1) in
      match v1 with
      | V_bool true ->
          let t2 = derive_exp e2 env in
          let v = result_of_judgement (root_of_tree t2) in
          Tree (J_evalto (env, e, v), E_IfT, [t1; t2])
      | V_bool false ->
          let t3 = derive_exp e3 env in
          let v = result_of_judgement (root_of_tree t3) in
          Tree (J_evalto (env, e, v), E_IfF, [t1; t3])
      | _ -> raise (Invalid_argument "Conditional must be bool.") )
  | BinOp (op, e1, e2) ->
      let t1 = derive_exp e1 env in
      let v1 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_exp e2 env in
      let v2 = result_of_judgement (root_of_tree t2) in
      let t3 = derive_nat_bin_op (op, v1, v2) in
      let v3 = result_of_judgement (root_of_tree t3) in
      Tree (J_evalto (env, e, v3), rule_of_op op, [t1; t2; t3])
  | E_let (x, e1, e2) ->
      let t1 = derive_exp e1 env in
      let v1 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_exp e2 ((x, v1) :: env) in
      let v = result_of_judgement (root_of_tree t2) in
      Tree (J_evalto (env, e, v), E_Let, [t1; t2])
  | E_fun (x, e') -> Tree (J_evalto (env, e, V_fun (env, x, e')), E_Fun, [])
  | E_app (e1, e2) -> (
      let t1 = derive_exp e1 env in
      let v1 = result_of_judgement (root_of_tree t1) in
      let t2 = derive_exp e2 env in
      let v2 = result_of_judgement (root_of_tree t2) in
      match v1 with
      | V_fun (env2, x, e0) ->
          let t3 = derive_exp e0 ((x, v2) :: env2) in
          let v = result_of_judgement (root_of_tree t3) in
          Tree (J_evalto (env, e, v), E_App, [t1; t2; t3])
      | V_letrec (env2, x, y, e0) ->
          let t3 = derive_exp e0 ((y, v2) :: (x, v1) :: env2) in
          let v = result_of_judgement (root_of_tree t3) in
          Tree (J_evalto (env, e, v), E_AppRec, [t1; t2; t3])
      | _ -> assert false )
  | E_letrec (x, y, e1, e2) ->
      let env2 = (x, V_letrec (env, x, y, e1)) :: env in
      let t = derive_exp e2 env2 in
      let v = result_of_judgement (root_of_tree t) in
      Tree (J_evalto (env, e, v), E_LetRec, [t])

and derive_nat_bin_op nat_bin_op =
  match nat_bin_op with
  | Plus, V_int i1, V_int i2 ->
      Tree (J_is (nat_bin_op, V_int (i1 + i2)), B_Plus, [])
  | Minus, V_int i1, V_int i2 ->
      Tree (J_is (nat_bin_op, V_int (i1 - i2)), B_Minus, [])
  | Times, V_int i1, V_int i2 ->
      Tree (J_is (nat_bin_op, V_int (i1 * i2)), B_Times, [])
  | Lt, V_int i1, V_int i2 ->
      Tree (J_is (nat_bin_op, V_bool (i1 < i2)), B_Lt, [])
  | _ -> raise (Invalid_argument "No evaluation rule can be applied.")

let derive = function
  | J_evalto (env, e, _) -> derive_exp e env
  | J_is (t, _) -> derive_nat_bin_op t
