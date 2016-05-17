open Ast;;
open Interpreter;;
open Typecheck;;
open Printf;;

let myHeap = [];;
let myEnv = [];;

(*** Interpreter ***)

(* [interp e] first type checks [e] in the empty 
   environment.  If that succeeds, then [interp]
   evaluates [e] to a value [v], where e -->* v.
   That evaluation should never raise an exception,
   becuase [e] typechecks. *)
let interp e env heap =
  (*ignore(typecheck empty e); multistep e*)
  multistep e env heap


(* A few test cases *)
let assert_raises f x =
  try ignore (f x); false with
  | Failure _ -> true
  | _ -> false
  
let _ = assert (Prim (Int 22) = (interp (Prim (Int 22)) myEnv myHeap));;

let _ = print_int (getIntVal (interp (Prim (Int 22)) myEnv myHeap));;
printf "\n";;

let _ = assert (Prim (Int 22) = interp (Add(Prim (Int 11), Prim(Int 11)))  myEnv myHeap)

let _ = assert ( true = is_value (Int 22 ))
let _ = assert ( true = is_primitive (Prim (Int 10)))
let _ = assert ( 1 = getFirstStack [1;2;3])

let _ = printList (addStack( addStack (addStack [] 1) 2) 3);;
printf "\n";;
let _ = print_int (getFirstStack (addStack( addStack (addStack [] 1) 2) 3));;
printf "\n";;
let _ = printList (addStack [1;2;3;4] 6);;
printf "\n";;
let _ = print_int (getIntVal (Prim (Int 10)));;
printf "\n";;
let _ = printf "%b" (getBoolVal (Prim (Bool true)));;
printf "\n";;

(* Check environment vars *)

let _ = assert ( getFirstVarName ( 
	 [  ("myVar", TypeVal (TPrimitive, (Int 5))) ] ) = "myVar" )

let _ = assert ( getFirstVarValue ( 
	 [  ("myVar", TypeVal (TPrimitive, (Int 5))) ] ) = (Int 5) )

let _ = assert ( getFirstVarValue ([ 
								  	 ("myVar1", TypeVal (TPrimitive, (Int 5)));
								  	 ("myVar2", TypeVal (TPrimitive, (Int 6)));
								  	 ("myVar3", TypeVal (TPrimitive, (Int 7)));
								  ]) = (Int 5) )

let _ = assert ( getFirstVarValue ( popFirstVar ( popFirstVar ([ 
								  	 ("myVar1", TypeVal (TPrimitive, (Int 5)));
								  	 ("myVar2", TypeVal (TPrimitive, (Int 6)));
								  	 ("myVar3", TypeVal (TPrimitive, (Int 7)));
								  ]))) = (Int 7) )

let empty = ref [];;

(************  Typecheck tests   *************)
(* PRIM *)
let _ = assert ( TInt = typecheck ref empty (Prim (Int 5)))
let _ = assert ( TFloat = typecheck ref empty (Prim (Float 5.0)))
let _ = assert ( TBool = typecheck ref empty (Prim (Bool true)))
let _ = assert ( TUnit = typecheck ref empty (Prim Unit))

(* ADD *)
let _ = assert ( TInt = typecheck ref empty (Add(Prim(Int 11), Prim(Int 11))))
let _ = assert ( TFloat = typecheck ref empty (Add(Prim(Float 11.0), Prim(Float 11.0))))

let _ = assert (true = assert_raises (typecheck ref empty) (Add(Prim(Int 11), Prim(Float 11.0))))

(* SUB *)
let _ = assert ( TInt = typecheck ref empty (Sub(Prim(Int 11), Prim(Int 11))))
let _ = assert ( TFloat = typecheck ref empty (Sub(Prim(Float 11.0), Prim(Float 11.0))))

let _ = assert (true = assert_raises (typecheck ref empty) (Sub(Prim(Int 11), Prim(Float 11.0))))

(* MULT *)
let _ = assert ( TInt = typecheck ref empty (Mult(Prim(Int 11), Prim(Int 11))))
let _ = assert ( TFloat = typecheck ref empty (Mult(Prim(Float 11.0), Prim(Float 11.0))))

let _ = assert (true = assert_raises (typecheck ref empty) (Mult(Prim(Int 11), Prim(Float 11.0))))

(* DIV *)
let _ = assert ( TInt = typecheck ref empty (Div(Prim(Int 11), Prim(Int 11))))
let _ = assert ( TFloat = typecheck ref empty (Div(Prim(Float 11.0), Prim(Float 11.0))))

let _ = assert (true = assert_raises (typecheck ref empty) (Div(Prim(Int 11), Prim(Float 11.0))))

(* AND *)
let _ = assert ( TBool = typecheck ref empty (And(Prim(Bool true), Prim(Bool true))))
let _ = assert (true = assert_raises (typecheck ref empty) (And(Prim(Bool true), Prim(Int 5))))

(* OR *)
let _ = assert ( TBool = typecheck ref empty (Or(Prim(Bool true), Prim(Bool true))))
let _ = assert (true = assert_raises (typecheck ref empty) (Or(Prim(Bool true), Prim(Int 5))))

(* NOT *)
let _ = assert ( TBool = typecheck ref empty (Not(Prim(Bool true))))
let _ = assert (true = assert_raises (typecheck ref empty) (Not(Prim(Int 5))) )


(* Equals *)
let _ = assert ( TBool = typecheck ref empty (Equals(Prim(Int 11), Prim(Int 11))))
let _ = assert ( TBool = typecheck ref empty (Equals(Prim(Float 11.0), Prim(Float 11.0))))

let _ = assert (true = assert_raises (typecheck ref empty) (Equals(Prim(Int 11), Prim(Float 11.0))))

(* NotEquals *)
let _ = assert ( TBool = typecheck ref empty (NotEquals(Prim(Int 11), Prim(Int 11))))
let _ = assert ( TBool = typecheck ref empty (NotEquals(Prim(Float 11.0), Prim(Float 11.0))))

let _ = assert (true = assert_raises (typecheck ref empty) (NotEquals(Prim(Int 11), Prim(Float 11.0))))

(* Less *)
let _ = assert ( TBool = typecheck ref empty (Less(Prim(Int 11), Prim(Int 11))))
let _ = assert ( TBool = typecheck ref empty (Less(Prim(Float 11.0), Prim(Float 11.0))))

let _ = assert (true = assert_raises (typecheck ref empty) (Less(Prim(Int 11), Prim(Float 11.0))))

(* LessOrEquals *)
let _ = assert ( TBool = typecheck ref empty (LessOrEquals(Prim(Int 11), Prim(Int 11))))
let _ = assert ( TBool = typecheck ref empty (LessOrEquals(Prim(Float 11.0), Prim(Float 11.0))))

let _ = assert (true = assert_raises (typecheck ref empty) (LessOrEquals(Prim(Int 11), Prim(Float 11.0))))

(* Greater *)
let _ = assert ( TBool = typecheck ref empty (Grater(Prim(Int 11), Prim(Int 11))))
let _ = assert ( TBool = typecheck ref empty (Grater(Prim(Float 11.0), Prim(Float 11.0))))

let _ = assert (true = assert_raises (typecheck ref empty) (Grater(Prim(Int 11), Prim(Float 11.0))))

(* GreaterOrEquals *)
let _ = assert ( TBool = typecheck ref empty (GraterOrEquals(Prim(Int 11), Prim(Int 11))))
let _ = assert ( TBool = typecheck ref empty (GraterOrEquals(Prim(Float 11.0), Prim(Float 11.0))))

let _ = assert ( true = assert_raises (typecheck ref empty) (GraterOrEquals(Prim(Int 11), Prim(Float 11.0))))
