%{
open System.Compare_nat2.Compare_nat2
%}

%token S Z
%token LPAREN RPAREN
%token IS LESS THAN
%token EOL
%start toplevel
%type <judgement> toplevel
%%

toplevel:
    | j=judgement EOL                { j }
;

judgement:
    | n1=nat IS LESS THAN n2=nat { J_less(n1, n2) }
;

nat:
    | atom     { $1 }
    | S atom { S $2 }
;

atom:
    | Z { Z }
    | LPAREN n=nat RPAREN { n }
