type tPrim = TInt | TFloat | TBool | TUnit

type tVal = 
 | Unit 
 | Int of int
 | Float of float
 | Bool of bool
 | Evoid
 | Loc of int

type typ = 
    | TPrimitive of tVal
    | TClass of string
    | TBot  

type expr =
  | Prim of tVal
  | Var of string
  | Field of string*string
  | AssignVar of string*expr
  | AssignField of string*string*expr
  | Sequence of expr*expr
  | Compound of expr*expr 
  | If of expr*expr*expr
  | Add of expr*expr
  | Sub of expr*expr
  | Mult of expr*expr
  | Div of expr*expr
  | And of expr*expr
  | Or of expr*expr
  | Not of expr
  | While of expr*expr
  | BlockWithVar of typ*string*expr
  | BlockWithoutVar of expr

type fieldDecl = FieldDecl of typ*string

type fieldDeclList = FieldDeclList of fieldDecl list

type parameter = Parameter of typ*string

type parameterList = ParameterList of parameter list

type methodDecl = MethodDecl of typ*string*parameterList*expr

type methodDeclList = MethodDeclList of methodDecl list

type classDecl = ClassDecl of string*string*fieldDeclList*methodDeclList

type prog = Prog of (string*classDecl) list

(* --------- *)
(*    Alex   *)
(* --------- *)

(*Environment vars*)
type typVal = TypeVal of typ*tVal

(* This should be a stack *)
type varEnv = VarEnv of string*typVal list
(*-------------------*)


(*Heap vars*)
type fieldEnv = FieldEnv of string*typVal list

type objVal = ObjVal of string*fieldEnv 

type heap = Heap of int*objVal
(*-------------------*)


