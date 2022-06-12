module type System = sig
  type judgement
  type rule
  type t
  type subst = (int * t) list
  type eqs = (t * t) list

  val inversion : judgement -> rule * judgement list * eqs
  val eqs_of_subst : subst -> eqs
  val unify : eqs -> subst
  val one_subst_judgement : int * t -> judgement -> judgement
  val string_of_judgement : judgement -> string
  val string_of_rule : rule -> string
end
