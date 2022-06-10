%{
open System.Nameless_ml3.System
%}

%token <int> VINT
%token <bool> VBOOL
%token <string> VAR
%token PLUS MINUS TIMES LT
%token IF THEN ELSE 
%token LPAREN RPAREN
%token PROVE COMMA
%token LET EQ IN
%token FUN REC ARROW
%token COMPILE SHARP DOT
%token EOL
%start toplevel
%type <judgement> toplevel
%%

toplevel:
    | j=judgement EOL { j }

judgement:
    | ev=env PROVE e=exp COMPILE d=db_exp { J_compile(ev, e, d) }

env:
    | { [] }
    | ev=env COMMA x=VAR { x :: ev }
    | x=VAR { [x] }

exp:
    | l=lt_exp { l }
    | s=special_exp { s }

special_exp:
    | i=if_exp { i }
    | l=let_exp { l }
    | f=fun_exp { f }
    | r=let_rec { r }

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
    | t=times_exp TIMES a=app_exp  { BinOp(Times, t, a) }
    | t=times_exp TIMES i=if_exp { BinOp(Times, t, i) }
    | a=app_exp { a }

app_exp :
    | a1=app_exp a2=a_exp { E_app (a1, a2) }
    | a=a_exp { a}

a_exp:
    | i=VINT              { E_int  i }
    | b=VBOOL             { E_bool b }
    | x=VAR { E_var x }
    | LPAREN e=exp RPAREN { e }

let_exp:
    | LET x=VAR EQ e1=exp IN e2=exp { E_let(x, e1, e2) }

fun_exp:
    | FUN x=VAR ARROW e=exp { E_fun(x, e) }

let_rec:
    | LET REC x=VAR EQ FUN y=VAR ARROW e1=exp IN e2=exp { E_letrec(x, y, e1, e2)}

db_exp:
    | l=db_lt_exp { l }
    | s=db_special_exp { s }

db_special_exp:
    | i=db_if_exp { i }
    | l=db_let_exp { l }
    | f=db_fun_exp { f }
    | r=db_let_rec { r }

db_if_exp:
    | IF e1=db_exp THEN e2=db_exp ELSE e3=db_exp { DB_if(e1, e2, e3) }

db_lt_exp:
    | p1=db_plus_exp LT p2=db_plus_exp { DB_BinOp(Lt, p1, p2) }
    | p=db_plus_exp  LT  i=db_if_exp   { DB_BinOp(Lt, p, i)   }
    | p=db_plus_exp                 { p }

db_plus_exp:
    | p=db_plus_exp PLUS  t=db_times_exp { DB_BinOp(Plus,  p, t) }
    | p=db_plus_exp PLUS  i=db_if_exp    { DB_BinOp(Plus,  p, i) }
    | p=db_plus_exp MINUS t=db_times_exp { DB_BinOp(Minus, p, t) }
    | p=db_plus_exp MINUS i=db_if_exp    { DB_BinOp(Minus, p, i) }
    | t=db_times_exp                  { t }

db_times_exp:
    | t=db_times_exp TIMES a=db_app_exp  { DB_BinOp(Times, t, a) }
    | t=db_times_exp TIMES i=db_if_exp { DB_BinOp(Times, t, i) }
    | a=db_app_exp { a }

db_app_exp :
    | a1=db_app_exp a2=db_a_exp { DB_app (a1, a2) }
    | a=db_a_exp { a }

db_a_exp:
    | i=VINT              { DB_int  i }
    | b=VBOOL             { DB_bool b }
    | SHARP n=VINT { DB_var n }
    | LPAREN e=db_exp RPAREN { e }

db_let_exp:
    | LET DOT EQ e1=db_exp IN e2=db_exp { DB_let(e1, e2) }

db_fun_exp:
    | FUN DOT ARROW e=db_exp { DB_fun e }

db_let_rec:
    | LET REC DOT EQ FUN DOT ARROW e1=db_exp IN e2=db_exp { DB_letrec(e1, e2)}
