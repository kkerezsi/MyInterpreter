open Ast;;
open Interpreter;;

(* [lookup c x] is the type of [x] according to context [c]. *)
let lookup ctx x =
  try List.assoc x ctx with
  | Not_found -> failwith "Type error (unbound variable)"

(* [extend c x t] is the same context as [c], but with [x] bound
   to type [t].  If [x] was already bound, its previous binding
   is shadowed by [t]. *)
let extend ctx x t =
  (x,t)::ctx


let rec typecheck ctx = function
  | GetVal x -> lookup ctx x
  (* | AssignVar(name, e1) -> typecheck_assign_var ctx name e1 *)
  | Prim n -> typecheck_prim n
  | Add(e1,e2) -> typecheck_add ctx e1 e2
  | Sub(e1, e2) -> typecheck_sub ctx e1 e2
  | Mult(e1, e2) -> typecheck_mult ctx e1 e2
  | Div(e1, e2) -> typecheck_div ctx e1 e2
  | And(e1,e2) -> typecheck_and ctx e1 e2
  | Or(e1, e2) -> typecheck_or ctx e1 e2
  | Equals(e1, e2) | NotEquals(e1, e2) | Less(e1, e2) | LessOrEquals(e1, e2) | GraterOrEquals(e1, e2) | Grater(e1, e2) -> typecheck_comparison ctx e1 e2
  | Not e1 -> typecheck_not ctx e1
  (* | If(e1,e2,e3) -> typecheck_if ctx e1 e2 e3 *)
  | _ ->failwith "Type check error"

and

(*******  Prim  ********)
  typecheck_prim n =
  match n with
  | Int _ -> TInt
  | Float _ -> TFloat
  | Unit -> TUnit
  | Evoid -> TUnit
  | Bool _ -> TBool
  | Loc _ -> TInt
  | _ -> failwith "Type error (Prim)"

and

(*******  ADD  ********)
  typecheck_add ctx e1 e2 =
  match (typecheck ctx e1, typecheck ctx e2) with
    | (TInt,TInt) -> TInt
    | (TFloat, TFloat) -> TFloat
    | _ -> failwith "Type error (add)"

and

(*******  SUB  ********)
  typecheck_sub ctx e1 e2 =
  match (typecheck ctx e1, typecheck ctx e2) with
    | (TInt,TInt) -> TInt
    | (TFloat, TFloat) -> TFloat
    | _ -> failwith "Type error (sub)"

and

(*******  MULT  ********)
  typecheck_mult ctx e1 e2 =
  match (typecheck ctx e1, typecheck ctx e2) with
    | (TInt,TInt) -> TInt
    | (TFloat, TFloat) -> TFloat
    | _ -> failwith "Type error (mult)"

and

(*******  DIV  ********)
  typecheck_div ctx e1 e2 =
  match (typecheck ctx e1, typecheck ctx e2) with
    | (TInt,TInt) -> TInt
    | (TFloat, TFloat) -> TFloat
    | _ -> failwith "Type error (div)"

and

(*******  AND  ********)
  typecheck_and ctx e1 e2 =
  match (typecheck ctx e1, typecheck ctx e2) with
    | (TBool,TBool) -> TBool
    | _ -> failwith "Type error (and)"

and

(*******  OR  ********)
  typecheck_or ctx e1 e2 =
  match (typecheck ctx e1, typecheck ctx e2) with
    | (TBool,TBool) -> TBool
    | _ -> failwith "Type error (or)"

and

(*******  NOT  ********)
  typecheck_not ctx e1  =
  match typecheck ctx e1 with
    | TBool -> TBool
    | _ -> failwith "Type error (not)"


and

(*******  COMPARISON  ********)
  typecheck_comparison ctx e1 e2 =
  match (typecheck ctx e1, typecheck ctx e2) with
    | (TInt, TInt) -> TBool
    | (TFloat, TFloat) -> TBool
    | _ -> failwith "Type error (Comparison)"

(* and

(*******  AssignVar  ********)
  typecheck_assign_var ctx name e1  =
  let e1t = typecheck ctx e1 in
  let varType = extend ctx name e1t *)