open Ast;;
open Interpreter;;
(*type check*)
let rec typecheck ctx = function
  | Var x -> lookup ctx x
  | Int _ -> TInt
  | Bool _ -> TBool
  | Add(e1,e2) -> typecheck_add ctx e1 e2
  | And(e1,e2) -> typecheck_and ctx e1 e2
  | If(e1,e2,e3) -> typecheck_if ctx e1 e2 e3
  | _ ->failwith "Type check error"

and

  typecheck_add ctx e1 e2 =
  match (typecheck ctx e1, typecheck ctx e2) with
    | (TInt,TInt) -> TInt
    | _ -> failwith "Type error (add)"

and

  typecheck_and ctx e1 e2 =
  match (typecheck ctx e1, typecheck ctx e2) with
    | (TBool,TBool) -> TBool
    | _ -> failwith "Type error (and)"

and

  (*typecheck_let ctx x t e1 e2 =
    let e1t = typecheck ctx e1 in
    let e2t = typecheck (extend ctx x t) e2 in
    if e1t = t then e2t
    else failwith Type error (let) 

    and  ////out of order*)

  typecheck_if ctx e1 e2 e3 =
  let e1t = typecheck ctx e1 in
  let e2t = typecheck ctx e2 in
  let e3t = typecheck ctx e3 in
    if e1t = TBool && e2t = e3t then e2t
    else failwith "Type error (if)"
