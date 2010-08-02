open Misc
open Syntax

exception Typing_error of string
module SubstTypeVar = Map.Make (struct
                                  type t = string
                                  let compare = String.compare
                                end)
  
let err s = raise (Typing_error ("Typing error: "^s))

(**********************************************************************)  
(**********************************************************************)
(***************             PRETTY PRINTER              **************)
(**********************************************************************)
(**********************************************************************)  
(* substition type variables by strings for pretty printer *)
let rec subst_of_typevar subst next =
  let get_next next =
    let len = String.length next in
    let last = len - 1 in
    let src_c = String.get next last in
    let s =
      if src_c = 'z' then
        let new_s = String.create @< len + 1 in
        String.blit next 0 new_s 0 len;
        String.set new_s len 'a';
        new_s
      else
        let new_s = String.create len in
        let src_i = int_of_char src_c in
        String.set new_s last @< char_of_int @< src_i + 1;
        new_s
    in
    s
  in 
  function
    VarT id ->
      assert (SubstTypeVar.mem id subst);
      subst, next
  | IntT    -> (subst, next)
  | FunT (ptyp, btyp) ->
      let (subst', next') = subst_of_typevar subst next ptyp  in
      subst_of_typevar subst' next' btyp
  | TypeFunT (id, btyp) ->
      let newsubst = SubstTypeVar.add id next subst in
      subst_of_typevar newsubst (get_next next) btyp
              
let pps_typ typ =
  let rec pp subst = function
      VarT id -> (try SubstTypeVar.find id subst with _ -> assert false)
    | IntT    -> "int"
    | FunT (ptyp, btyp) ->
        let f = (match ptyp with
                   FunT _ | TypeFunT _ -> (fun x -> "("^x^")")
                 | _                   -> (fun x -> x))
        in
        f (pp subst ptyp)^" -> "^(pp subst btyp)
    | TypeFunT (id, btyp) -> "'"^(try SubstTypeVar.find id subst with _ -> assert false)^" => "^(pp subst btyp)
  in
  let (subst, _) = subst_of_typevar SubstTypeVar.empty "a" typ in
  pp subst typ
let pp_typ typ = print_string @< pps_typ typ
  
(**********************************************************************)
(**********************************************************************)
(***************                 TYPING                  **************)
(**********************************************************************)
(**********************************************************************)  
(* for type application *)
let rec subst s dtyp = function
    VarT id -> if id = s then dtyp else VarT id
  | IntT -> IntT
  | FunT (ptyp, btyp) -> FunT (subst s dtyp ptyp, subst s dtyp btyp)
  | TypeFunT (id, btyp) -> TypeFunT (id, if id = s then btyp else subst s dtyp btyp)

(* check that a type is valid *)
let rec valid_type types = function
    VarT id -> Env.mem types id
  | IntT    -> true
  | FunT (ptyp, btyp) -> valid_type types ptyp && valid_type types btyp 
  | TypeFunT (id, btyp) ->
      let types' = Env.extend types id @< VarT id in
      valid_type types' btyp
      
let rec typing_binop = function
    (Plus, IntT, IntT) -> IntT
  | (Mult, IntT, IntT) -> IntT
  | (Div,  IntT, IntT) -> IntT
  | (Plus, _,_)        -> err "the types of both arguments of + must be integer"
  | (Mult, _,_)        -> err "the types of both arguments of + must be integer"
  | (Div,  _,_)        -> err "the types of both arguments of + must be integer"
      
let rec typing_exp env types = function
    Var id -> (match Env.lookup env id with
                 Some t -> t
               | None   -> err @< Printf.sprintf "%s is not bound" id)
  | IntLit _ -> IntT
  | BinOp (op, el, er) ->
      let tl, tr = typing_exp env types el, typing_exp env types er in
      typing_binop (op, tl, tr)
  | Fun (para, typ, body) ->
      if valid_type types typ then
        let btyp = typing_exp (Env.extend env para typ) types body in
        FunT (typ, btyp)
      else err @< Printf.sprintf "refered type variables are not bound"
  | App (fexp, pexp) ->
      let ftyp, ptyp = typing_exp env types fexp, typing_exp env types pexp in
      (match ftyp with 
         FunT (ptyp', btyp) when ptyp = ptyp' -> btyp
       | FunT _ -> err "the types of a bounded variable and an actual parameter must be same"
       | _ -> err "the function doesn't have a function type")
  | TypeFun (tv, body) ->
      TypeFunT (tv, typing_exp env (Env.extend types tv @< VarT tv) body)
  | TypeApp (fexp, ptyp) ->
      if valid_type types ptyp then
        match typing_exp env types fexp with
          TypeFunT (tv, btype) -> subst tv ptyp btype
        | _ -> err "the type abstraction doesn't have a type of type abstraction"
      else err "refered type variables are not bound"
      
let typing = typing_exp
