%{
open System.Eval_nameless_ml3.System
%}

%token <int> VINT
%token <bool> VBOOL
%token PLUS MINUS TIMES LT
%token IF THEN ELSE 
%token LPAREN RPAREN
%token EVALTO
%token PROVE COMMA
%token LET EQ IN
%token LBRACKET RBRACKET
%token FUN REC ARROW
%token SHARP DOT
%token EOL
%start toplevel
%type <judgement> toplevel
%%

toplevel:
    | j=judgement EOL { j }

judgement:
    | ev=env PROVE d=db_exp EVALTO v=dbvalue { J_evalto(ev, d, v) }

env:
    | { [] }
    | ev=env COMMA v=dbvalue  { v :: ev }
    | v=dbvalue { [v] }

dbvalue:
    | i=VINT  { DBV_int  i }
    | b=VBOOL { DBV_bool b }
    | LPAREN ev=env RPAREN LBRACKET FUN DOT ARROW d=db_exp RBRACKET { DBV_fun(ev, d)}
    | LPAREN ev=env RPAREN LBRACKET REC DOT EQ FUN DOT ARROW d=db_exp RBRACKET { DBV_letrec(ev, d)}

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
