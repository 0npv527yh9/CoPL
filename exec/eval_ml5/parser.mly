%{
open System.Eval_ml5.System
%}

%token <int> VINT
%token <bool> VBOOL
%token <string> VAR
%token PLUS MINUS TIMES LT
%token IF THEN ELSE 
%token LPAREN RPAREN
%token EVALTO
%token PROVE COMMA
%token LET EQ IN
%token LBRACKET RBRACKET
%token FUN REC ARROW
%token CONS MATCH WITH VBAR UNDERSCORE
%token EOL
%start toplevel
%type <judgement> toplevel
%%

toplevel:
    | j=judgement EOL { j }

judgement:
    | ev=env PROVE e=exp EVALTO v=value { J_evalto(ev, e, v) }

env:
    | { [] }
    | ev=env COMMA x=VAR EQ v=value  { (x, v) :: ev }
    | x=VAR EQ v=value { [(x, v)] }

value:
    | i=VINT  { V_int  i }
    | b=VBOOL { V_bool b }
    | LPAREN ev=env RPAREN LBRACKET FUN x=VAR ARROW e=exp RBRACKET { V_fun(ev, x, e)}
    | LPAREN ev=env RPAREN LBRACKET REC x=VAR EQ FUN y=VAR ARROW e=exp RBRACKET { V_letrec(ev, x, y, e)}
    | LBRACKET RBRACKET { V_nil }
    | v1=value CONS v2=value { V_cons(v1, v2 ) }

exp:
    | l=lt_exp { l }
    | s=special_exp { s }

special_exp:
    | i=if_exp { i }
    | l=let_exp { l }
    | f=fun_exp { f }
    | r=let_rec { r }
    | m=match_exp { m }

lt_exp:
    | c1=cons_exp LT c2=cons_exp { BinOp(Lt, c1, c2) }
    | c=cons_exp  LT  s=special_exp   { BinOp(Lt, c, s)   }
    | c=cons_exp                 { c }

cons_exp:
    | p=plus_exp CONS c=cons_exp { E_cons(p, c) }
    | p=plus_exp CONS s=special_exp { E_cons(p, s) }
    | p=plus_exp { p }

plus_exp:
    | p=plus_exp PLUS  t=times_exp { BinOp(Plus,  p, t) }
    | p=plus_exp PLUS  s=special_exp   { BinOp(Plus, p, s)   }
    | p=plus_exp MINUS t=times_exp { BinOp(Minus, p, t) }
    | p=plus_exp MINUS s=special_exp   { BinOp(Minus, p, s)   }
    | t=times_exp                  { t }

times_exp:
    | t=times_exp TIMES a=app_exp  { BinOp(Times, t, a) }
    | t=times_exp TIMES s=special_exp   { BinOp(Times, t, s)   }
    | a=app_exp { a }

app_exp :
    | a1=app_exp a2=a_exp { E_app (a1, a2) }
    | a=a_exp { a }

a_exp:
    | i=VINT              { E_int  i }
    | b=VBOOL             { E_bool b }
    | x=VAR { E_var x }
    | LPAREN e=exp RPAREN { e }
    | LBRACKET RBRACKET { E_nil }

if_exp:
    | IF e1=exp THEN e2=exp ELSE e3=exp { E_if(e1, e2, e3) }

let_exp:
    | LET x=VAR EQ e1=exp IN e2=exp { E_let(x, e1, e2) }

fun_exp:
    | FUN x=VAR ARROW e=exp { E_fun(x, e) }

let_rec:
    | LET REC x=VAR EQ FUN y=VAR ARROW e1=exp IN e2=exp { E_letrec(x, y, e1, e2)}

match_exp:
    | MATCH e1=exp WITH c=clause { E_match(e1, c) }

clause:
    | p=pattern ARROW e=exp { C_one(p, e) }
    | p=pattern ARROW e=exp VBAR c=clause { C_seq(p, e, c) }

pattern:
    | p1=pattern_one CONS p2=pattern { P_cons(p1, p2) }
    | p=pattern_one { p }

pattern_one:
    | x=VAR { P_var x }
    | LBRACKET RBRACKET { P_nil }
    | UNDERSCORE { P_wild }
    | LPAREN p=pattern RPAREN { p }
