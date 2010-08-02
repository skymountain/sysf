%{
  open Syntax
    
  let reserv_types = [
    "int", IntT;
  ]

  let type_of_string s =
    match OptionList.assoc s reserv_types with
      None -> VarT  s
    | Some t -> t
%}

%token BACKSLA BACKSLA2 COLON DOT SEMICOLON2
%token LSQPAREN RSQPAREN RARROW
%token LPAREN RPAREN  
%token PLUS ASTER SLASH
%token<Syntax.id> IDENT APOSTIDENT
%token<int> INTLIT

%left PLUS
%left ASTER SLASH
  
%start main
%type<Syntax.program> main
%%
  
main:
  Expr SEMICOLON2 { $1 }

Expr:
  BACKSLA IDENT COLON FunExpr DOT Expr { Fun ($2, $4, $6) }
| BACKSLA2 IDENT DOT Expr { TypeFun ($2, $4) }
| ArithExpr { $1 }
      
ArithExpr:      
  ArithExpr PLUS  ArithExpr  { BinOp (Plus, $1, $3) }
| ArithExpr ASTER ArithExpr { BinOp (Mult, $1, $3) }
| ArithExpr SLASH ArithExpr { BinOp (Div,  $1, $3) }
| AppExpr         { $1 }
      
AppExpr:
  AppExpr AtomExpr { App ($1, $2) }
| AppExpr LSQPAREN FunExpr RSQPAREN { TypeApp ($1, $3) }
| AtomExpr { $1 }

AtomExpr:
  INTLIT { IntLit $1 }
| IDENT  { Var $1 }
| LPAREN Expr RPAREN { $2 }

FunExpr:
  AtomFunExpr RARROW FunExpr { FunT ($1, $3) }
| APOSTIDENT RARROW FunExpr { TypeFunT ($1, $3) }
| AtomFunExpr { $1 }

AtomFunExpr:
  IDENT { type_of_string $1 }
| LPAREN FunExpr RPAREN { $2 }
