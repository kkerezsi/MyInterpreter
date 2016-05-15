open Ast;;
open Interpreter;;

(* 
type tPrim = TInt | TFloat | TBool | TUnit

type tVal = 
 | Unit (* ok *)
 | Int of int (* ok *)
 | Float of float (* ok *)
 | Bool of bool (* ok *)
 | Evoid (* ok *)
 | Loc of int (* ok *)

type typ = 
    | TPrimitive
    | TClass
    | TBot  

(* --------- *)
(*    Alex   *)
(* --------- *)

(*Environment vars*)
type typVal = TypeVal of typ*tVal

(* This should be a stack *)
type varEnv = VarEnv of (string*typVal) list
(*-------------------*)

(*Heap vars*)
type fieldEnv = FieldEnv of string*typVal list

type objVal = ObjVal of string*fieldEnv 

type heap = Heap of (int*objVal) list
(*-------------------*)

type expr =
  | Prim of tVal 
  | Var of string (* ok *)
  | Field of string*string (**)
  | AssignVar of string*expr (* ok *)
  | AssignField of string*string*expr (**)
  | Sequence of expr*expr 
  | If of expr*expr*expr 
  | Add of expr*expr (* ok *)
  | Sub of expr*expr (* ok *)
  | Mult of expr*expr (* ok *)
  | Div of expr*expr  (* ok *)
  | And of expr*expr (* ok *)
  | Or of expr*expr (* ok *)
  | Not of expr (* ok *)
  | While of expr*expr 
  | BlockWithVar of typ*string*expr (**)
  | BlockWithoutVar of expr 
  | Ret of string*tVal (* *)

type fieldDecl = FieldDecl of typ*string

type fieldDeclList = FieldDeclList of fieldDecl list

type parameter = Parameter of typ*string

type parameterList = ParameterList of parameter list

type methodDecl = MethodDecl of typ*string*parameterList*expr

type methodDeclList = MethodDeclList of methodDecl list

type classDecl = ClassDecl of string*string*fieldDeclList*methodDeclList

type prog = Prog of (string*classDecl) list
*)
(*type check *)

(* The empty context. *)
let empty = []

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
  | Var x -> lookup ctx x
  | Prim n -> typecheck_prim n
  | Add(e1,e2) -> typecheck_add ctx e1 e2
  | Sub(e1, e2) -> typecheck_sub ctx e1 e2
  | Mult(e1, e2) -> typecheck_mult ctx e1 e2
  | Div(e1, e2) -> typecheck_div ctx e1 e2
  | And(e1,e2) -> typecheck_and ctx e1 e2
  | Or(e1, e2) -> typecheck_or ctx e1 e2
  | Not e1 -> typecheck_not ctx e1
  (* | If(e1,e2,e3) -> typecheck_if ctx e1 e2 e3 *)
  (* | AssignVar(name, e1) -> typecheck_assign_var ctx name e1 *)
  | _ ->failwith "Type check error"

and

(*******  ADD  ********)
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

(* and

(*******  AssignVar  ********)
  typecheck_assign_var ctx name e1  =
  let e1t = typecheck ctx e1 in
  let varType = extend ctx name e1t *)