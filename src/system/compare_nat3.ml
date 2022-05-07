module Compare_nat3 = struct
  type nat = Z | S of nat
  type judgement = J_less of nat * nat
  type rule = L_Succ | L_SuccR

  module String = struct
    let rec of_nat = function Z -> "Z" | S n -> "S(" ^ of_nat n ^ ")"

    let of_judgement = function
      | J_less (n1, n2) -> of_nat n1 ^ " is less than " ^ of_nat n2

    let of_rule = function L_Succ -> "L-Succ" | L_SuccR -> "L-SuccR"
  end

  let string_of_judgement = String.of_judgement
  let string_of_rule = String.of_rule
end

module Tree = Core.Derivation_tree.Make (Compare_nat3)
open Compare_nat3
open Tree

let rec derive judgement =
  match judgement with
  | J_less (n, n') when n' = S n -> Tree (judgement, L_Succ, [])
  | J_less (n1, S n2) ->
      let t = derive (J_less (n1, n2)) in
      Tree (judgement, L_SuccR, [t])
  | _ ->
      raise
        (Invalid_argument (string_of_judgement judgement ^ " does not hold."))
