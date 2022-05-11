%{
open System.Nat.System
%}

%token S Z
%token LPAREN RPAREN
%token PLUS TIMES IS
%token EOL
%start toplevel
%type <judgement> toplevel
%%

toplevel:
    | j=judgement EOL                { j }

judgement:
    | n1=nat op=op n2=nat IS n3=nat { J_is(Bin_op(op, n1, n2), n3) }

nat:
    | Z     { Z }
    | S atom { S $2 }

atom:
    | Z { Z }
    | LPAREN n=nat RPAREN { n }

op:
    | PLUS { Plus }
    | TIMES { Times }
