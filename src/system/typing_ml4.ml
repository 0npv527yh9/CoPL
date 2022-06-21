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
    | E_nil
    | E_cons of exp * exp
    | E_match of exp * exp * var * var * exp

  type type_ =
    | T_bool
    | T_int
    | T_fun of type_ * type_
    | T_list of type_
    | V of int

  type env = (var * type_) list
  type judgement = J_typing of env * exp * type_

  type rule =
    | T_Int
    | T_Bool
    | T_If
    | T_Plus
    | T_Minus
    | T_Times
    | T_Lt
    | T_Var
    | T_Let
    | T_Fun
    | T_App
    | T_LetRec
    | T_Nil
    | T_Cons
    | T_Match

  let fresh_int =
    let counter = ref ~-1 in
    let body () =
      counter := !counter + 1 ;
      !counter in
    body

  let rule_of_op = function
    | Plus -> T_Plus
    | Minus -> T_Minus
    | Times -> T_Times
    | Lt -> T_Lt

  let inversion = function
    | J_typing (_, E_int _, t) -> (T_Int, [], [(t, T_int)])
    | J_typing (_, E_bool _, t) -> (T_Bool, [], [(t, T_bool)])
    | J_typing (env, E_if (e1, e2, e3), t) ->
        ( T_If
        , [ J_typing (env, e1, T_bool); J_typing (env, e2, t)
          ; J_typing (env, e3, t) ]
        , [] )
    | J_typing (env, BinOp (op, e1, e2), t) ->
        let t' = match op with Lt -> T_bool | _ -> T_int in
        ( rule_of_op op
        , [J_typing (env, e1, T_int); J_typing (env, e2, T_int)]
        , [(t, t')] )
    | J_typing (env, E_var x, t) -> (T_Var, [], [(List.assoc x env, t)])
    | J_typing (env, E_let (x, e1, e2), t2) ->
        let t1 = V (fresh_int ()) in
        (T_Let, [J_typing (env, e1, t1); J_typing ((x, t1) :: env, e2, t2)], [])
    | J_typing (env, E_fun (x, e), t) ->
        let t1 = V (fresh_int ()) in
        let t2 = V (fresh_int ()) in
        (T_Fun, [J_typing ((x, t1) :: env, e, t2)], [(t, T_fun (t1, t2))])
    | J_typing (env, E_app (e1, e2), t2) ->
        let t1 = V (fresh_int ()) in
        (T_App, [J_typing (env, e1, T_fun (t1, t2)); J_typing (env, e2, t1)], [])
    | J_typing (env, E_letrec (x, y, e1, e2), t) ->
        let t1 = V (fresh_int ()) in
        let t2 = V (fresh_int ()) in
        ( T_LetRec
        , [ J_typing ((y, t1) :: (x, T_fun (t1, t2)) :: env, e1, t2)
          ; J_typing ((x, T_fun (t1, t2)) :: env, e2, t) ]
        , [] )
    | J_typing (_, E_nil, t) ->
        let t' = V (fresh_int ()) in
        (T_Nil, [], [(t, T_list t')])
    | J_typing (env, E_cons (e1, e2), t) ->
        let t' = V (fresh_int ()) in
        ( T_Cons
        , [J_typing (env, e1, t'); J_typing (env, e2, t)]
        , [(t, T_list t')] )
    | J_typing (env, E_match (e1, e2, x, y, e3), t) ->
        let t' = V (fresh_int ()) in
        ( T_Match
        , [ J_typing (env, e1, T_list t'); J_typing (env, e2, t)
          ; J_typing ((y, T_list t') :: (x, t') :: env, e3, t) ]
        , [] )

  type t = type_
  type eqs = (t * t) list
  type subst = (int * t) list

  let rec one_subst_type (i, t) = function
    | (T_bool | T_int) as t' -> t'
    | T_fun (t1, t2) ->
        T_fun (one_subst_type (i, t) t1, one_subst_type (i, t) t2)
    | T_list t' -> T_list (one_subst_type (i, t) t')
    | V i' -> if i = i' then t else V i'

  let rec unify = function
    | [] -> []
    | (t, t') :: eqs -> (
        if t = t' then unify eqs
        else
          match (t, t') with
          | T_fun (t1, t2), T_fun (t1', t2') ->
              unify ((t1, t1') :: (t2, t2') :: eqs)
          | T_list t, T_list t' -> unify ((t, t') :: eqs)
          | t, V i | V i, t ->
              let f = one_subst_type (i, t) in
              (i, t) :: unify (List.rev_map (fun (t1, t2) -> (f t1, f t2)) eqs)
          | _ -> assert false )

  let rec one_subst_env s = function
    | [] -> []
    | (x, t) :: env -> (x, one_subst_type s t) :: one_subst_env s env

  let one_subst_judgement s = function
    | J_typing (env, e, t) ->
        let env' = one_subst_env s env in
        let t' = one_subst_type s t in
        J_typing (env', e, t')

  (* D / がんばったら derivation_tree にもっていける V 次第 / create_var ? *)
  let eqs_of_subst subst = List.rev_map (fun (i, n) -> (V i, n)) subst

  module String = struct
    let of_op = function Plus -> "+" | Minus -> "-" | Times -> "*" | Lt -> "<"

    let rec of_type = function
      | T_bool -> "bool"
      | T_int -> "int"
      | T_fun (t1, t2) ->
          let s1 =
            match t1 with T_fun _ -> "(" ^ of_type t1 ^ ")" | _ -> of_type t1
          in
          s1 ^ " -> " ^ of_type t2
      | T_list t ->
          let s =
            match t with T_fun _ -> "(" ^ of_type t ^ ")" | _ -> of_type t in
          s ^ " list"
      | V _ -> of_type T_bool
    (* 多相的でないため，自由変数があればboolに *)

    let rec paren_special = function
      | ( E_if _ | E_let _ | E_fun _ | E_app _ | E_letrec _ | E_cons _
        | E_match _ ) as e ->
          "(" ^ of_exp e ^ ")"
      | e -> of_exp e

    and of_exp = function
      | E_int i -> string_of_int i
      | E_bool b -> string_of_bool b
      | E_var x -> x
      | BinOp (Lt, e1, e2) ->
          paren_special e1 ^ " " ^ of_op Lt ^ " " ^ of_exp e2
      | BinOp (((Plus | Minus) as op), e1, e2) ->
          let s2 =
            match e2 with
            | BinOp ((Plus | Minus), _, _) -> "(" ^ of_exp e2 ^ ")"
            | _ -> of_exp e2 in
          paren_special e1 ^ " " ^ of_op op ^ " " ^ s2
      | BinOp (Times, e1, e2) ->
          let s1 =
            match e1 with
            | BinOp ((Plus | Minus), _, _) -> "(" ^ of_exp e1 ^ ")"
            | _ -> paren_special e1 in
          let s2 =
            match e2 with
            | BinOp (_, _, _) -> "(" ^ of_exp e2 ^ ")"
            | _ -> of_exp e2 in
          s1 ^ " " ^ of_op Times ^ " " ^ s2
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
      | E_nil -> "[]"
      | E_cons (e1, e2) ->
          let s1 =
            match e1 with
            | E_cons _ -> "(" ^ of_exp e1 ^ ")"
            | _ -> paren_special e1 in
          s1 ^ " :: " ^ of_exp e2
      | E_match (e1, e2, x, y, e3) ->
          "match " ^ of_exp e1 ^ " with [] -> " ^ of_exp e2 ^ " | " ^ x ^ " :: "
          ^ y ^ " -> " ^ of_exp e3

    and of_env = function
      | [] -> ""
      | [(x, t)] -> x ^ " : " ^ of_type t
      | (x, t) :: env -> of_env env ^ ", " ^ x ^ " : " ^ of_type t

    let of_judgement = function
      | J_typing (env, e, t) ->
          of_env env ^ " |- " ^ of_exp e ^ " : " ^ of_type t

    let of_rule = function
      | T_Int -> "T-Int"
      | T_Bool -> "T-Bool"
      | T_If -> "T-If"
      | T_Plus -> "T-Plus"
      | T_Minus -> "T-Minus"
      | T_Times -> "T-Times"
      | T_Lt -> "T-Lt"
      | T_Var -> "T-Var"
      | T_Let -> "T-Let"
      | T_Fun -> "T-Fun"
      | T_App -> "T-App"
      | T_LetRec -> "T-LetRec"
      | T_Nil -> "T-Nil"
      | T_Cons -> "T-Cons"
      | T_Match -> "T-Match"
  end

  let string_of_judgement = String.of_judgement
  let string_of_rule = String.of_rule
end

module Tree = Core.Inference_derivation_tree.Make (System)
