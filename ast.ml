type tPrim = TInt | TFloat | TBool | TVoid

type typ = 
    | TPrimitive of tPrim
    | TClass of string
    | TBot  (* ASK what this is *)

type expr =
    | Primitive of typ
    | Unit of unit
    | Int of int 
    | Float of float
    | Bool of bool
    | Var of string
    | Field of string*string
    (* Below is the equivalent of "Statements" from the MAP interpreter  *)
    | AssignVar of string*expr
    | AssignField of string*string*expr
    (*| Declr of field*expr *)
    | Compound of expr*expr 
    | If of expr*expr*expr
    | Add of expr*expr
    | Sub of expr*expr
    | Mult of expr*expr
    | Div of expr*expr
    | And of expr*expr
    | Or of expr*expr
    | Not of expr
    (*| New of string*field list (*instanciates a new class member, followed by atributes*) *)
    (*| Call of string*string*field list (*class member and method name and list of parameters*) *)
    | While of expr*expr

type fieldDecl = FieldDecl of typ*string

type fieldDeclList = FieldDeclList of fieldDecl list

type parameter = Parameter of typ*string

type parameterList = ParameterList of parameter list

type methodDecl = MethodDecl of typ*string*parameterList*expr

type methodDeclList = MethodDeclList of methodDecl list

type classDecl = ClassDecl of string*string*fieldDeclList*methodDeclList

type prog = Prog of (string*classDecl) list