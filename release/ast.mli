(******************************************************************************)
(** expressions ***************************************************************)
(******************************************************************************)

(** variable names (e.g. "map" or "x") *)
type var         = string

(** constructor names (e.g. "Some" or "Cons") *)
type constructor = string

(** the binary operators that can be included in OCaml programs.
 *
 * NOTE: comparison operators (>, <. =, >=, <=, <>) only operate on integers
 *)
type operator =
  | Plus | Minus | Times                (** +, -, * *)
  | Gt | Lt | Eq | GtEq | LtEq | NotEq  (** >, <, =, >=, <=, <> *)
  | Concat                              (** ^ *)

(** (OCaml) values of type expr represent OCalf expressions. *)
type expr =
  | Unit                               (** Unit                 represents () *)
  | Int      of int                    (** Int 7                represents 7  *)
  | Bool     of bool                   (** Bool true            represents true *)
  | String   of string                 (** String "hello"       represents "hello" *)
  | BinOp    of operator * expr * expr (** Binop (Plus, e1, e2) represents e1 + e2 *)
  | If       of expr * expr * expr     (** If (e1,e2,e3)        represents if e1 then e2 else e3 *)
  | Var      of var                    (** Var "x"              represents the variable x *)
  | Let      of var * expr * expr      (** Let    ("x",e1,e2)   represents let x = e1 in e2 *)
  | LetRec   of var * expr * expr      (** LetRec ("x",e1,e2)   represents let rec x = e1 in e2 *)
  | App      of expr * expr            (** App (e1,e2)          represents (e1 e2) *)
  | Fun      of var  * expr            (** Fun (x,e)            represents (fun x -> e) *)
  | Pair     of expr * expr            (** Pair (e1,e2)         represents (e1,e2)  *)
  | Variant  of constructor * expr     (** Variant ("Cons",e)   represents Cons e (here Cons could be any constructor name) *)
  | Match    of expr * (pattern * expr) list
				       (** Match (e, [(p1,e1);(p2,e2);...])
					     represents
					   match e with | p1 -> e1 | p2 -> e1 | ... *)

(** patterns are non-function values with some subvalues replaced by variables *)
and pattern =
  | PUnit
  | PInt     of int
  | PBool    of bool
  | PString  of string
  | PVar     of var
  | PVariant of constructor * pattern
  | PPair    of pattern * pattern


