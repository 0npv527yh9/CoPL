%{
open System.Typing_ml4.System
%}

%token <int> VINT
%token <bool> VBOOL
%token <string> VAR
%token PLUS MINUS TIMES LT
%token IF THEN ELSE 
%token LPAREN RPAREN
%token PROVE COMMA
%token LET EQ IN
%token LBRACKET RBRACKET
%token FUN REC ARROW
%token CONS MATCH WITH VBAR
%token CORON
%token TINT TBOOL TLIST
%token EOL
%start toplevel
%type <judgement> toplevel
%%

toplevel:
    | j=judgement EOL { j }

judgement:
    | ev=env PROVE e=exp CORON t=type_ { J_typing(ev, e, t) }

env:
    | { [] }
    | ev=env COMMA x=VAR CORON t=type_ { (x, t) :: ev }
    | x=VAR CORON t=type_ { [(x, t)] }

type_:
    | t1=list_type ARROW t2=type_ { T_fun(t1, t2) }
    | t=list_type { t }

list_type:
    | t=list_type TLIST { T_list t }
    | t=type_one { t }

type_one:
    | TINT { T_int }
    | TBOOL { T_bool }
    | LPAREN t=type_ RPAREN { t }
    | i=VINT { V i }

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
    | MATCH e1=exp WITH LBRACKET RBRACKET ARROW e2=exp VBAR x=VAR CONS y=VAR ARROW e3=exp { E_match(e1, e2, x, y, e3) } 
