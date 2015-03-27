open Ast
open TypedAst

type equation = Eq of typ * typ

(******************************************************************************)
(** type substitution *********************************************************)
(******************************************************************************)

(**
 * These are useful functions for applying a substitution of a type for a type
 * variable
 *)

(** A substitution of a type for a type variable *)
type substitution = talpha * typ

(** apply a type substitution to a type *)
let rec subst_typ ((x,t'):substitution) (t:typ) =
  match t with
  | TAlpha y
      -> if y = x then t' else TAlpha y
  | TUnit | TInt | TBool | TString
      -> t
  | TArrow (t1,t2)
      -> TArrow (subst_typ (x,t') t1, subst_typ (x,t') t2)
  | TStar (t1,t2)
      -> TStar (subst_typ (x,t') t1, subst_typ (x,t') t2)
  | TVariant (ts, name)
      -> TVariant (List.map (subst_typ (x,t')) ts, name)

(** apply a type substitution to a list of equations *)
let subst_eqn (s : substitution) (eqns : equation list) : equation list =
  List.map (fun (Eq (t1,t2)) -> Eq(subst_typ s t1, subst_typ s t2)) eqns

(** apply a type substitution to an annotated expression *)
let rec subst_expr (s : substitution) (e : annotated_expr) : annotated_expr =
  match e with
  | AVar      (t,x)            -> AVar      (subst_typ s t, x)
  | AApp      (t,e1,e2)        -> AApp      (subst_typ s t, subst_expr s e1, subst_expr s e2)
  | AFun      (t,(x,tx),e)     -> AFun      (subst_typ s t, (x, subst_typ s tx), subst_expr s e)
  | ALet      (t,(x,tx),e1,e2) -> ALet      (subst_typ s t, (x, subst_typ s tx), subst_expr s e1, subst_expr s e2)
  | ALetRec   (t,(x,tx),e1,e2) -> ALetRec   (subst_typ s t, (x, subst_typ s tx), subst_expr s e1, subst_expr s e2)
  | AUnit     (t)              -> AUnit     (subst_typ s t)
  | AInt      (t,n)            -> AInt      (subst_typ s t, n)
  | ABool     (t,b)            -> ABool     (subst_typ s t, b)
  | AString   (t,k)            -> AString   (subst_typ s t, k)
  | AVariant  (t,c,e)          -> AVariant  (subst_typ s t, c, subst_expr s e)
  | APair     (t,e1,e2)        -> APair     (subst_typ s t, subst_expr s e1, subst_expr s e2)
  | ABinOp    (t,op,e1,e2)     -> ABinOp    (subst_typ s t, op, subst_expr s e1, subst_expr s e2)
  | AIf       (t,e1,e2,e3)     -> AIf       (subst_typ s t, subst_expr s e1, subst_expr s e2, subst_expr s e3)
  | AMatch    (t,e,ps)         -> AMatch    (subst_typ s t, subst_expr s e, List.map (subst_case s) ps)
and subst_case s (p,e) = subst_pat s p, subst_expr s e
and subst_pat  s = function
  | APUnit    (t)              -> APUnit    (subst_typ s t)
  | APInt     (t,n)            -> APInt     (subst_typ s t, n)
  | APBool    (t,b)            -> APBool    (subst_typ s t, b)
  | APString  (t,k)            -> APString  (subst_typ s t, k)
  | APVar     (t,x)            -> APVar     (subst_typ s t, x)
  | APVariant (t,c,p)          -> APVariant (subst_typ s t, c, subst_pat s p)
  | APPair    (t,p1,p2)        -> APPair    (subst_typ s t, subst_pat s p1, subst_pat s p2)

(******************************************************************************)
(** helper functions **********************************************************)
(******************************************************************************)

(* you may find it helpful to implement these or other helper
 * functions, but they are not required.  Feel free to implement them if you
 * need them, change their types or arguments, delete them, whatever.
 *)


(** Format a list of equations for printing. *)
let format_eqns (f : Format.formatter) (eqns : equation list) : unit =
  (* see the comment in Eval.format_value for guidance implementing hints *)
  failwith "I may not have been sure about what really did interest me,
            but I was absolutely sure about what I didn't."

(** use format_eqns to print a value to the console *)
let print_eqns     = Printer.make_printer format_eqns

(** use format_value to convert a value to a string *)
let string_of_eqns = Printer.make_string_of format_eqns




(** generate an unused type variable *)
let newvar () : typ =
  failwith "I opened myself to the gentle indifference of the world."



(* return the constraints for a binary operator *)
let collect_binop (t:typ) (op:operator) (tl:typ) (tr:typ) : equation list =
  match op with
  | Plus | Minus | Times 
    -> [Eq (t,TInt); Eq (tl,TInt); Eq (tr,TInt)] 
  | Gt | Lt | Eq | GtEq | LtEq | NotEq 
    -> [Eq (t,TBool); Eq (tl,TInt); Eq (tr,TInt)]
  | Concat 
    -> [Eq (t,TString); Eq (tl,TString); Eq (tr,TString)]

(** return the constraints for an expr
  * vars refers to a data structure that stores the types of each of the variables
  * that have been defined.
  * It is completely your decision what type of data structure you want to use for vars
  *)
type vars = (var * typ ref) list

let rec collect_expr (specs:variant_spec list) (vars: vars) (e : annotated_expr)
                     : equation list =
  match e with
  | AUnit (t) -> [Eq (t, TUnit)]
  | AInt (t,n) -> [Eq (t,TInt)]
  | ABool (t,b) -> [Eq (t, TBool)]
  | AString (t,k) -> [Eq (t,TString)]
  | AVar (t,x) -> [Eq (t, !(List.assoc x vars))]

  | AApp (t,e1,e2) -> (collect_expr specs vars e1)
                      @(collect_expr specs vars e2)

  | ALet (t,(x,tx),e1,e2) -> [Eq (t,typeof e2); Eq (tx, (typeof e1))]
                              @(collect_expr specs vars e1)
                              @(collect_expr specs ((x, ref tx)::vars) e2)

  | ALetRec (t,(x,tx),e1,e2) -> [Eq (t,typeof e2); Eq (tx, (typeof e1))]
                                @(collect_expr specs ((x, ref tx)::vars) e1)
                                @(collect_expr specs ((x, ref tx)::vars) e2)

  |APair (t,e1,e2) -> [Eq (t, TStar (typeof e1, typeof e2))]
                      @(collect_expr specs vars e1)
                      @(collect_expr specs vars e2)

  | ABinOp (t,o,e1,e2) -> (collect_binop t o (typeof e1) (typeof e2))
                          @(collect_expr specs vars e1)
                          @(collect_expr specs vars e2)

  | AFun (t,(x,tx),e) -> [Eq (t, TArrow (tx,(typeof e)))]
                          @(collect_expr specs ((x, ref tx)::vars) e) 

  | AIf (t, e1, e2, e3) -> [Eq (t, typeof e2); Eq (typeof e1, TBool);
                            Eq (typeof e2, typeof e3)]
                           @(collect_expr specs vars e1)
                           @(collect_expr specs vars e2)
                           @(collect_expr specs vars e3) 

  | AMatch (t,m,[(p,e)]) -> [Eq (t, typeof e)]
                             @(fst (collect_pat specs p))
                             @(collect_expr [] vars m)
      @(collect_case specs ((snd (collect_pat specs p))@vars) (typeof m) t (p,e)) 

  | AMatch (t,m,((p,e)::tl)) -> [Eq (t, typeof e)] (* type of AMatch = output*)
                                @(fst (collect_pat specs p)) (*pattern constraints*)
                                @(collect_expr [] vars m) (*constraints of item being matched *)
      @(collect_case specs ((snd (collect_pat specs p))@vars) (typeof m) t (p,e))
      (* ^ get constraints for case (p,e), using vars + vars assigned by pattern *) 
  | _ -> failwith "nope"

(** return the constraints for a match cases
  * tconst refers to the type of the parameters of the specific constructors
  * tvariant refers to the type of the variant as a whole
  *)
and collect_case specs vs tconst tvariant ((p:annotated_pattern),(e:annotated_expr)) =
  match e with 
  | AInt (t,n) -> [Eq (t,TInt)]
  | ABool (t,b) -> [Eq (t,TBool)]
  | AString (t,k) -> [Eq (t,TString)]
  | AUnit (t) -> [Eq (t,TUnit)]
  | AVar (t,x) -> [Eq (t,!(List.assoc x vs))]
  | APair (t,e1,e2) -> [Eq (t, TStar (typeof e1, typeof e2))] 
  | _ -> failwith "nope"

(** return the constraints and variables (var list?) for a pattern *)
and collect_pat specs (p:annotated_pattern) =
  match p with 
  | APUnit (t) -> ([Eq (t, TUnit)],[])
  | APInt (t,n) -> ([Eq (t, TInt)],[])
  | APBool (t,b) -> ([Eq (t, TBool)],[])
  | APString (t,k) -> ([Eq (t, TString)],[]) 
  | APVar (t,x) -> ([],[(x,ref t)])
  | APPair (t,p1,p2) -> ([Eq (t, TStar (typeof_pat p1, typeof_pat p2))],[])
  | _ -> failwith "nope"
  

(******************************************************************************)
(** constraint generation                                                    **)
(******************************************************************************)

(**
 * collect traverses an expression e and returns a list of equations that must
 * be satisfied for e to typecheck.
 *)

let collect (specs: variant_spec list) (e: annotated_expr) : equation list = 
  match e with
  | AUnit t -> [Eq (t,TUnit)]
  | AInt (t,n) -> [Eq (t,TInt)]
  | ABool (t,b) -> [Eq (t,TBool)]
  | AString (t,k) -> [Eq (t,TString)]
  | AVar (t,x) -> []
  | AApp (t,e1,e2) -> collect_expr specs [] (AApp (t,e1,e2))
  | AFun (t,(x,tx),e) 
    -> [Eq (t,TArrow (tx, (typeof e)))]@(collect_expr specs [(x,ref tx)] e) 
  | ALet (t,(x,tx),e1,e2) -> collect_expr specs [] (ALet (t,(x,tx),e1,e2))
  | ALetRec (t,(x,tx),e1,e2) -> collect_expr specs [] (ALetRec (t,(x,tx),e1,e2))
  | APair (t,e1,e2) -> collect_expr specs [] (APair (t,e1,e2))
  | ABinOp (t,o,e1,e2) -> collect_expr specs [] (ABinOp (t,o,e1,e2))
  | AIf (t,e1,e2,e3) -> collect_expr specs [] (AIf (t,e1,e2,e3))
  | AMatch (t,m,l) -> collect_expr specs [] (AMatch (t,m,l))
  | _ -> failwith "nope"

(******************************************************************************)
(** constraint solver (unification)                                          **)
(******************************************************************************)

let rec occurs_in x = function
  | TAlpha y
      -> x = y
  | TArrow (t1,t2) | TStar (t1,t2)
      -> occurs_in x t1 || occurs_in x t2
  | TVariant (ts,_)
      -> List.exists (occurs_in x) ts
  | TUnit | TInt | TBool | TString
      -> false

(**
 * unify solves a system of equations and returns a list of
 * definitions for the type variables.
 *)
let rec unify eqns = match eqns with
  | [] -> []

  | Eq (t1,t2)::tl when t1 = t2
     -> unify tl

  | Eq ((TAlpha x as t1), (t as t2))::tl
  | Eq ((t as t1), (TAlpha x as t2))::tl
     -> if occurs_in x t
        then failwith (Format.asprintf "circular type constraint %a = %a"
                                       Printer.format_type t1
                                       Printer.format_type t2)
        else (x,t)::(unify (subst_eqn (x,t) tl))

  | Eq (TArrow (t1,t1'), TArrow (t2,t2'))::tl
  | Eq (TStar  (t1,t1'), TStar  (t2,t2'))::tl
     -> unify ((Eq (t1,t2))::(Eq (t1',t2'))::tl)

  | Eq ((TVariant (t1s, n1) as t1), (TVariant (t2s, n2) as t2))::tl
     -> if n1 <> n2
        then failwith (Format.asprintf "can't unify %a and %a"
                                       Printer.format_type t1
                                       Printer.format_type t2)
        else unify ((List.map2 (fun t1 t2 -> Eq (t1,t2)) t1s t2s)
                    @ tl)

  | Eq (t1,t2)::tl
     -> failwith (Format.asprintf "can't unify %a and %a"
                                  Printer.format_type t1
                                  Printer.format_type t2)

(******************************************************************************)
(** inference                                                                **)
(******************************************************************************)

(**
 * rename the type variables so that the first is "a", the
 * second "b", and so on.  Example:
 *
 *  rename_vars ('t23 -> 't17 -> 't23 -> int)
 *  is          ('a   -> 'b   -> 'a   -> int)
 *)
let rec simplify e =
  let rec alpha_of_int i =
    let let_of_int i = String.make 1 (char_of_int (i - 1 + int_of_char 'a')) in
    if i <= 0 then "" else (alpha_of_int (i/26))^(let_of_int (i mod 26))
  in

  let next_var  = ref 0 in

  let newvar () =
    next_var := 1 + !next_var;
    TAlpha (alpha_of_int !next_var)
  in

  let rec subst vars = function
    | TAlpha x -> if List.mem_assoc x vars then vars else (x,newvar())::vars
    | TUnit | TInt | TBool | TString -> vars
    | TArrow (t1,t2) | TStar (t1,t2) -> let vars' = subst vars t1 in
                                        subst vars' t2
    | TVariant (ts,_) -> List.fold_left subst vars ts
  in

  subst [] e

(**
 * given an expression, return the type for that expression,
 * failing if it cannot be typed.
 *)
let infer defs e =
  let annotated = annotate e in
  let eqns      = collect defs annotated in
  let solution  = unify eqns in
  let newtype   = List.fold_left (fun e s -> subst_expr s e) annotated solution in
  let simplify  = simplify (typeof newtype) in
  List.fold_right subst_expr simplify newtype
