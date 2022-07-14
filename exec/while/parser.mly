%{
open System.While.System
%}

%token <int> VINT
%token <bool> VBOOL
%token <string> VAR
%token PLUS MINUS TIMES 
%token NOT AND OR 
%token LT EQ LE
%token IF THEN ELSE 
%token LPAREN RPAREN
%token COMMA
%token SKIP ASSIGN SEMI 
%token WHILE DO
%token CHANGES TO
%token EOL
%start toplevel
%type <judgement> toplevel
%%

toplevel:
    | j=judgement EOL { j }

judgement:
    | c=command CHANGES s1=store TO s2=store { J_changes(c, s1, s2) }

command:
    | c1=command_atom SEMI c2=command { C_seq(c1, c2) }
    | c=command_atom { c }

command_atom:
    | SKIP { C_skip }
    | x=VAR ASSIGN a=aexp { C_assign(x, a) }
    | IF b=bexp THEN c1=command ELSE c2=command { C_if(b, c1, c2) }
    | WHILE LPAREN b=bexp RPAREN DO c=command { C_while(b, c) }

aexp:
    | a1=aexp PLUS a2=a_times { A_binop(Plus, a1, a2) }
    | a1=aexp MINUS a2=a_times { A_binop(Minus, a1, a2) }
    | a=a_times { a }

a_times:
    | a1=a_times TIMES a2=a_atom { A_binop(Times, a1, a2) }
    | a=a_atom { a }

a_atom:
    | i=VINT { A_int i }
    | x=VAR { A_var x }

bexp:
    | b1=bexp OR b2=b_and { B_binop(Or, b1, b2) }
    | b=b_and { b }

b_and:
    | b1=b_and AND b2=b_not { B_binop(And, b1, b2) }
    | b=b_not { b }

b_not:
    | NOT b=b_not { B_not b }
    | b=b_atom { b }

b_atom:
    | b=VBOOL { B_bool b }
    | a1=aexp c=comp a2=aexp { B_comp(c, a1, a2) }

comp:
    | LT { Lt }
    | EQ { Eq }
    | LE { Le }

store:
    | { [] }
    | s=store COMMA x=VAR EQ i=VINT  { (x, i) :: s }
    | x=VAR EQ i=VINT { [(x, i)] }
