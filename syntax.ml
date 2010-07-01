type id = string

type binop =
    Plus
  | Mult
  | Div

type typ =
    IntT
  | FunT

type exp =
    Var     of id
  | IntLit  of int
  | BinOp   of binop * exp * exp
  | Fun     of id * string (* type *) * exp
  | App     of exp * exp
  | TypeFun of id * exp
      
type program = exp
