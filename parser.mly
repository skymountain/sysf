%{
  open Syntax
    
  let reserv_types = [
    "int", IntT;
  ]

  let type_of_string s =
    match OptionList.assoc s reserv_types with
      None -> VarT  s
    | Some t -> t

  let rec substitute x exp body = match body with
      Var y when x = y -> exp
    | BinOp (op, lexp, rexp) -> BinOp (op, substitute x exp lexp, substitute x exp rexp)
    | Fun (y, t, f) when x <> y -> Fun (y, t, substitute x exp f)
    | App (f, arg) -> App (substitute x exp f, substitute x exp arg)
    | TypeFun (y, f) -> TypeFun (y, substitute x exp f)
    | TypeApp (f, arg) -> TypeApp (substitute x exp f, arg)
    | _ -> body
        
%}

%token BACKSLA BACKSLA2 COLON DOT SEMICOLON2
%token LSQPAREN RSQPAREN RARROW
%token LPAREN RPAREN  
%token PLUS ASTER SLASH
%token<Syntax.id> IDENT APOSTIDENT
%token<int> INTLIT
%token EOF
%token LET IN EQUAL
  
%left PLUS
%left ASTER SLASH
  
%start main
%type<Syntax.program> main
%%
  
main:
  Expr SEMICOLON2 { Prog $1 }
| EOF { Syntax.EOF }
      
Expr:
  BACKSLA IDENT COLON FunExpr DOT Expr { Fun ($2, $4, $6) }
| BACKSLA2 IDENT DOT Expr { TypeFun ($2, $4) }
| LET IDENT EQUAL Expr IN Expr { substitute $2 $4 $6 }
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
