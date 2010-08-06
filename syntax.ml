type id = string

type binop =
    Plus
  | Mult
  | Div

type typ =
    IntT
  | VarT of id
  | FunT of typ * typ
  | TypeFunT of id * typ
      
type exp =
    Var     of id
  | IntLit  of int
  | BinOp   of binop * exp * exp
  | Fun     of id * typ * exp
  | App     of exp * exp
  | TypeFun of id * exp
  | TypeApp of exp * typ
      
type program =
    Prog of exp
  | EOF
