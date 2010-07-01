%{
  open Syntax
%}

%token BACKSLA BACKSLA2 COLON RARROW SEMICOLON2
%token LPAREN RPAREN  
%token PLUS ASTER SLASH
%token<Syntax.id> IDENT
%token<int> INTLIT

%left PLUS
%left ASTER SLASH
  
%start main
%type<Syntax.program> main
%%
  
main:
  Expr SEMICOLON2 { $1 }

Expr:
  Expr PLUS Expr  { BinOp (Plus, $1, $3) }
| Expr ASTER Expr { BinOp (Mult, $1, $3) }
| Expr SLASH Expr { BinOp (Div,  $1, $3) }
| AppExpr         { $1 }
      
AppExpr:
  AppExpr AtomExpr { App ($1, $2) } 
| AtomExpr { $1 }

AtomExpr:
  INTLIT { IntLit $1 }
| IDENT  { Var $1 }
| BACKSLA IDENT COLON IDENT RARROW Expr { Fun ($2, $4, $6) }
| BACKSLA2 IDENT RARROW Expr { TypeFun ($2, $4) }
| LPAREN Expr RPAREN { $2 }
