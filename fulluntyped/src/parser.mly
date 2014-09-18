%token <string> ID
%token LET IN
%token LAMBDA
%token DOT
%token EQ
%token L_PAR R_PAR
%token SEMICOL
%token EOF

(* %nonassoc LET *)
(* %nonassoc IN *)

%start <Syntax.term list> prog
%%

prog:
  | terms = separated_list(SEMICOL, term); EOF { terms }

term:
  | app = appterm                           { app }
  | LAMBDA; v = ID; DOT; b = term           { Syntax.LAbs (v, b) }
  (* | LET; v = ID; EQ; t = term %prec LET     { Syntax.LDef (v, t) }  *)
  | LET; v = ID; EQ; t = term; IN; b = term { Syntax.LLet (v, t, b) }

appterm:
  | a = appterm; t = aterm { Syntax.LApp (a, t) }
  | t = aterm              { t }
  
(* Atomic terms *)
aterm:
  | v = ID                 { Syntax.LVar v }
  | L_PAR; t = term; R_PAR { t }
