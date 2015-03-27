open Ast

(******************************************************************************)
(** types (see .mli) **********************************************************)
(******************************************************************************)

type value =
  | VUnit | VInt of int | VBool of bool | VString of string
  | VClosure of var * expr * environment
  | VVariant of constructor * value
  | VPair of value * value
  | VError of string
and environment = (var * value ref) list

(* Extract contents of environment option for use in eval*)
let match_opt o =
  match o with
  | None -> []
  | Some (e) -> e
(* Takes a pattern and value and returns resulting bindings (if any) as
 * an environment option. None denotes no match found *)
let rec find_match (p : pattern) (v : value) : environment option =
  match (p,v) with
  | (PUnit, VUnit)         -> Some ([]) 
  | (PInt a, VInt b)       -> if a == b then Some ([]) else None
  | (PBool a, VBool b)     -> if a == b then Some ([]) else None 
  | (PString a, VString b) -> if a == b then Some ([]) else None
  | (PVar x, _)            -> Some([x, ref v])  
  | (PVariant (c,p'), VVariant (c',v')) -> if c = c' then find_match p' v'
                                           else None  
  | (PPair (a,b),VPair(a',b')) -> let ao = find_match a a' 
                                  and bo = find_match b b' in 
                                  if (ao != None) && (bo != None) 
                                  then Some ((match_opt ao)@(match_opt bo)) 
                                  else None
  | _ -> None

(** apply the given operator to the given arguments *)
let rec eval_operator (op : operator) (v1 : value) (v2 : value) : value =
  match (op,v1,v2) with
  | (Plus, VInt x, VInt y)         -> VInt (x+y)
  | (Minus, VInt x, VInt y)        -> VInt (x-y)
  | (Times, VInt x, VInt y)        -> VInt (x*y)
  | (Gt, VInt x, VInt y)           -> VBool (x>y)
  | (Lt, VInt x, VInt y)           -> VBool (x<y)
  | (Eq, VInt x, VInt y)           -> VBool (x==y)
  | (GtEq, VInt x, VInt y)         -> VBool (x>=y)
  | (LtEq, VInt x, VInt y)         -> VBool (x<=y)
  | (NotEq, VInt x, VInt y)        -> VBool (x<>y)
  | (Concat, VString x, VString y) -> VString (x^y)
  | _                              -> VError "bad types man :("
  
(** Format a value for printing. *)
let rec format_value (f : Format.formatter) (v : value) : unit =
  failwith "I'm going to smile, and my smile will sink down into your pupils,
            and heaven knows what it will become."

(** use format_value to print a value to the console *)
let print_value = Printer.make_printer format_value

(** use format_value to convert a value to a string *)
let string_of_value = Printer.make_string_of format_value

(******************************************************************************)
(** eval **********************************************************************)
(******************************************************************************)


let rec eval (env: environment) (e: expr) : value = 
  match e with
  | Unit            -> VUnit
  | Int      x      -> VInt (x)
  | Bool     x      -> VBool (x)
  | String   x      -> VString (x) 
  | BinOp   (o,a,b) -> eval_operator o (eval env a) (eval env b)
  | If      (a,b,c) -> if (VBool true) = (eval env a) 
                       then (eval env b) 
                       else (eval env c)  
  | Var     x       -> !(List.assoc x env)
  | Fun     (v,a)   -> VClosure ((v), (a), env)
  | Pair    (a,b)   -> VPair ((eval env a), (eval env b))
  | Variant (c,a)   -> VVariant (c, (eval env a))
  | Let     (v,a,b) -> eval ((v,(ref (eval [] a)))::env) b
  | LetRec  (v,a,b) -> let env' = ((v, ref (VInt 0))::env) in 
                         let a' = eval env' a in 
                         begin
                           (List.assoc v env') := a';
                           eval env' b
                         end
  | App     (a,b)          -> eval (("x", (ref (eval [] b)))::env) a
  | Match   (a, [(p,e)])   -> let env' = find_match p (eval env a) in
                              if env' == None 
                              then VError "no match" 
                              else eval ((match_opt env')@env) e
  | Match   (a,((p,e)::t)) -> let env' = find_match p (eval env a) in 
                              if env' == None 
                              then eval env (Match (a,t)) 
                              else eval ((match_opt env')@env) e
  | _ -> VError "Not valid expression"
