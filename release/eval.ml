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

let rec find_match (p : pattern) (v : value) : environment option =
  failwith "You are -- your life, and nothing else"

(** apply the given operator to the given arguments *)
let rec eval_operator (op : operator) (v1 : value) (v2 : value) : value =
  failwith "I never could bear the idea of anyone expecting something from me.
            It always made me want to do just the opposite."

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

let rec eval env e =
  failwith "L'enfer, c'est les autres."
