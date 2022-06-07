%{
open System.Eval_ml2.System
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

exp:
    | l=lt_exp { l }
    | s=special_exp { s }

special_exp:
    | i=if_exp { i }
    | l=let_exp { l }

if_exp:
    | IF e1=exp THEN e2=exp ELSE e3=exp { E_if(e1, e2, e3) }

lt_exp:
    | p1=plus_exp LT p2=plus_exp { BinOp(Lt, p1, p2) }
    | p=plus_exp  LT  i=if_exp   { BinOp(Lt, p, i)   }
    | p=plus_exp                 { p }

plus_exp:
    | p=plus_exp PLUS  t=times_exp { BinOp(Plus,  p, t) }
    | p=plus_exp PLUS  i=if_exp    { BinOp(Plus,  p, i) }
    | p=plus_exp MINUS t=times_exp { BinOp(Minus, p, t) }
    | p=plus_exp MINUS i=if_exp    { BinOp(Minus, p, i) }
    | t=times_exp                  { t }

times_exp:
    | t=times_exp TIMES a=a_exp  { BinOp(Times, t, a) }
    | t=times_exp TIMES i=if_exp { BinOp(Times, t, i) }
    | a=a_exp { a }

a_exp:
    | i=VINT              { E_int  i }
    | b=VBOOL             { E_bool b }
    | x=VAR { E_var x }
    | LPAREN e=exp RPAREN { e }

let_exp:
    | LET x=VAR EQ e1=exp IN e2=exp { E_let(x, e1, e2) }
