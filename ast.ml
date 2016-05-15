type tPrim = TInt | TFloat | TBool | TUnit

type tVal = 
 | Unit 
 | Int of int
 | Float of float
 | Bool of bool
 | Evoid
 | Loc of int

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
  | Prim of tVal (* ok *)
  | Var of string (* ok *)
  | Field of string*string (**)
  | AssignVar of string*expr (**)
  | AssignField of string*string*expr (**)
  | Sequence of expr*expr (* ok *)
  | If of expr*expr*expr (* ok *)
  | Add of expr*expr (* ok *)
  | Sub of expr*expr (* ok *)
  | Mult of expr*expr (* ok *)
  | Div of expr*expr (* ok *)
  | And of expr*expr (* ok *)
  | Or of expr*expr (* ok *)
  | Not of expr (* ok *)
  | Equals of expr*expr (* *)
  | NotEquals of expr*expr (* *)
  | Less of expr*expr (* *)
  | LessOrEquals of expr*expr (* *)
  | GraterOrEquals of expr*expr (* *)
  | Grater of expr*expr (* *)
  | While of expr*expr (* ok *)
  | BlockWithVar of typ*string*expr (**)
  | BlockWithoutVar of expr (* ok *)
  | Ret of string*tVal (* *)

type fieldDecl = FieldDecl of typ*string

type fieldDeclList = FieldDeclList of fieldDecl list

type parameter = Parameter of typ*string

type parameterList = ParameterList of parameter list

type methodDecl = MethodDecl of typ*string*parameterList*expr

type methodDeclList = MethodDeclList of methodDecl list

type classDecl = ClassDecl of string*string*fieldDeclList*methodDeclList

type prog = Prog of (string*classDecl) list