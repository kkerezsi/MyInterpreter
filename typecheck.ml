open Ast;;
open Interpreter;;

(* [lookup c x] is the type of [x] according to context [c]. *)
let lookup ref ctx x =
  try List.assoc x !ctx with
  | Not_found -> failwith "Type error (unbound variable)"

(* [extend c x t] is the same context as [c], but with [x] bound
   to type [t].  If [x] was already bound, its previous binding
   is shadowed by [t]. *)
let extend ref ctx x t =
  ctx := (x,t)::!ctx;
  t;;


let rec typecheck ref ctx = function
  | GetVal x -> lookup ref ctx x
  | AssignVar(name, e1) -> typecheck_assign_var ref ctx name e1 
  | Prim n -> typecheck_prim n
  | Sequence(e1, e2) -> typecheck_seq ref ctx e1 e2
  | Add(e1,e2) -> typecheck_add ref ctx e1 e2
  | Sub(e1, e2) -> typecheck_sub ref ctx e1 e2
  | Mult(e1, e2) -> typecheck_mult ref ctx e1 e2
  | Div(e1, e2) -> typecheck_div ref ctx e1 e2
  | And(e1,e2) -> typecheck_and ref ctx e1 e2
  | Or(e1, e2) -> typecheck_or ref ctx e1 e2
  | Equals(e1, e2) | NotEquals(e1, e2) | Less(e1, e2) | LessOrEquals(e1, e2) | GraterOrEquals(e1, e2) | Grater(e1, e2) -> typecheck_comparison ref ctx e1 e2
  | Not e1 -> typecheck_not ref ctx e1
  (* | If(e1,e2,e3) -> typecheck_if ref ctx e1 e2 e3 *)
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

and

(******** Sequence *******)
  typecheck_seq ref ctx e1 e2 =
  let _ = typecheck ref ctx e1 in
  let e2t = typecheck ref ctx e2 in
  (* e1;e2 -> typeof e2 *)
  e2t

and

(*******  ADD  ********)
  typecheck_add ref ctx e1 e2 =
  match (typecheck ref ctx e1, typecheck ref ctx e2) with
    | (TInt,TInt) -> TInt
    | (TFloat, TFloat) -> TFloat
    | _ -> failwith "Type error (add)"

and

(*******  SUB  ********)
  typecheck_sub ref ctx e1 e2 =
  match (typecheck ref ctx e1, typecheck ref ctx e2) with
    | (TInt,TInt) -> TInt
    | (TFloat, TFloat) -> TFloat
    | _ -> failwith "Type error (sub)"

and

(*******  MULT  ********)
  typecheck_mult ref ctx e1 e2 =
  match (typecheck ref ctx e1, typecheck ref ctx e2) with
    | (TInt,TInt) -> TInt
    | (TFloat, TFloat) -> TFloat
    | _ -> failwith "Type error (mult)"

and

(*******  DIV  ********)
  typecheck_div ref ctx e1 e2 =
  match (typecheck ref ctx e1, typecheck ref ctx e2) with
    | (TInt,TInt) -> TInt
    | (TFloat, TFloat) -> TFloat
    | _ -> failwith "Type error (div)"

and

(*******  AND  ********)
  typecheck_and ref ctx e1 e2 =
  match (typecheck ref ctx e1, typecheck ref ctx e2) with
    | (TBool,TBool) -> TBool
    | _ -> failwith "Type error (and)"

and

(*******  OR  ********)
  typecheck_or ref ctx e1 e2 =
  match (typecheck ref ctx e1, typecheck ref ctx e2) with
    | (TBool,TBool) -> TBool
    | _ -> failwith "Type error (or)"

and

(*******  NOT  ********)
  typecheck_not ref ctx e1  =
  match typecheck ref ctx e1 with
    | TBool -> TBool
    | _ -> failwith "Type error (not)"


and

(*******  COMPARISON  ********)
  typecheck_comparison ref ctx e1 e2 =
  match (typecheck ref ctx e1, typecheck ref ctx e2) with
    | (TInt, TInt) -> TBool
    | (TFloat, TFloat) -> TBool
    | _ -> failwith "Type error (Comparison)"

 and

(* ****** Assign ****** *)
 typecheck_assign_var ref ctx x e1 = 
 let e1t = typecheck ref ctx e1 in
 let _ = extend ref ctx x e1t in
 e1t
