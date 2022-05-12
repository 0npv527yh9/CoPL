%{
open System.Eval_nat_exp.System
%}

%token S Z
%token LPAREN RPAREN
%token IS NAT_PLUS NAT_TIMES
%token PLUS TIMES
%token EVALTO
%token EOL
%start toplevel
%type <judgement> toplevel
%%

toplevel:
    | j=judgement EOL                { j }

judgement:
    | e=exp EVALTO n=nat { J_evalto(e, n) }
    | n1=nat op=op n2=nat IS n3=nat { J_is(Nat.BinOp(op, n1, n2), n3) }

exp:
    | p=plus_exp { p }

plus_exp:
    | p=plus_exp PLUS t=times_exp { BinOp(Plus, p, t) }
    | t=times_exp { t }

times_exp:
    | t=times_exp TIMES a=a_exp { BinOp(Times, t, a)}
    | a=a_exp { a }

a_exp:
    | n=nat { Nat n }
    | LPAREN e=exp RPAREN { e }

nat:
    | Z     { Z }
    | S a_nat { S $2 }

a_nat:
    | Z { Z }
    | LPAREN n=nat RPAREN { n }

op:
    | NAT_PLUS { Nat.Plus }
    | NAT_TIMES { Nat.Times }
