(** 
  Lambda Club introduction to OCaml using expressions
*)

(** (4+3)+2  *)
(**
  {add, {add, 4, 3}, mult, 2}
*)


type expr =
    Add of expr * expr
  | Mult of expr * expr
  | Num of int
  | Var of string
  | Let of string * expr * expr

(** "foo" = 42 in  "foo" + 9
   51
 *)

let example = Add (Add (Num 4, Num 3), Num 2)

(** [("foo", 42); ("bar", 7)] *)
type environment = (string*int) list

let rec find p name =
  match p with
  | [] -> raise Not_found
  | ((n,v)::ps) ->
    if n = name then v
    else find ps name

let rec eval p e =
  match e with
  | Add (e1, e2) ->
    eval p e1 + eval p e2
  | Mult (e1, e2) ->
    eval p e1 * eval p e2
  | Num n ->
    n
  | Var name ->
    find p name
  | Let (name, value, expr) ->
    eval ((name,eval p value)::p) expr
