open Misc
open Syntax

exception Eval_error of string
  
type value =
    IntV of int
  | FunV of id * exp * (id * value) Env.t
  | TypeFunV of exp * (id * value) Env.t
let err s = raise (Eval_error (Printf.sprintf "Runtime error: %s" s))
      
let pps_val = function
    IntV i -> string_of_int i
  | FunV _ -> "<fun>"
  | TypeFunV _ -> "<type fun>"

let pp_val v =
  print_string $ pps_val v

let eval_binop = function
    (Plus, IntV vl, IntV vr) -> IntV (vl + vr)
  | (Mult, IntV vl, IntV vr) -> IntV (vl * vr)
  | (Div, IntV vl, IntV vr)  -> IntV (vl / vr)
  | (Plus, _, _) -> err "both arguments of + must be integer"
  | (Mult, _, _) -> err "both arguments of * must be integer"
  | (Div, _, _)  -> err "both arguments of / must be integer"
      
let rec eval_exp env = function
    Var id -> (match Env.lookup env id with
                 None -> err $ Printf.sprintf "%s is not bound" id
               | Some v -> v)
  | IntLit i -> IntV i
  | BinOp (op, el, er) -> eval_binop (op, eval_exp env el, eval_exp env er)
  | Fun (para, _, body) -> FunV (para, body, env)
  | App (f, act) ->
      let f = eval_exp env f in
      (match f with
         FunV (formal, body, env) ->
           let act = eval_exp env act in
           eval_exp $ Env.extend env formal act $ body
       | TypeFunV (body, env)     -> eval_exp env body
       | _                        -> err "Not-function value is applied")
  | TypeFun (_, exp) -> TypeFunV (exp, env)

let eval env decl = env, "it", eval_exp env decl
