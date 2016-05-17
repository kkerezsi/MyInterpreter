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
let _ = assert ( TInt = typecheck empty (Prim (Int 5)))
let _ = assert ( TFloat = typecheck empty (Prim (Float 5.0)))
let _ = assert ( TBool = typecheck empty (Prim (Bool true)))
let _ = assert ( TUnit = typecheck empty (Prim Unit))

(* ADD *)
let _ = assert ( TInt = typecheck empty (Add(Prim(Int 11), Prim(Int 11))))
let _ = assert ( TFloat = typecheck empty (Add(Prim(Float 11.0), Prim(Float 11.0))))

let _ = assert (true = assert_raises (typecheck empty) (Add(Prim(Int 11), Prim(Float 11.0))))

(* SUB *)
let _ = assert ( TInt = typecheck empty (Sub(Prim(Int 11), Prim(Int 11))))
let _ = assert ( TFloat = typecheck empty (Sub(Prim(Float 11.0), Prim(Float 11.0))))

let _ = assert (true = assert_raises (typecheck empty) (Sub(Prim(Int 11), Prim(Float 11.0))))

(* MULT *)
let _ = assert ( TInt = typecheck empty (Mult(Prim(Int 11), Prim(Int 11))))
let _ = assert ( TFloat = typecheck empty (Mult(Prim(Float 11.0), Prim(Float 11.0))))

let _ = assert (true = assert_raises (typecheck empty) (Mult(Prim(Int 11), Prim(Float 11.0))))

(* DIV *)
let _ = assert ( TInt = typecheck empty (Div(Prim(Int 11), Prim(Int 11))))
let _ = assert ( TFloat = typecheck empty (Div(Prim(Float 11.0), Prim(Float 11.0))))

let _ = assert (true = assert_raises (typecheck empty) (Div(Prim(Int 11), Prim(Float 11.0))))

(* AND *)
let _ = assert ( TBool = typecheck empty (And(Prim(Bool true), Prim(Bool true))))
let _ = assert (true = assert_raises (typecheck empty) (And(Prim(Bool true), Prim(Int 5))))

(* OR *)
let _ = assert ( TBool = typecheck empty (Or(Prim(Bool true), Prim(Bool true))))
let _ = assert (true = assert_raises (typecheck empty) (Or(Prim(Bool true), Prim(Int 5))))

(* NOT *)
let _ = assert ( TBool = typecheck empty (Not(Prim(Bool true))))
let _ = assert (true = assert_raises (typecheck empty) (Not(Prim(Int 5))) )


(* Equals *)
let _ = assert ( TBool = typecheck empty (Equals(Prim(Int 11), Prim(Int 11))))
let _ = assert ( TBool = typecheck empty (Equals(Prim(Float 11.0), Prim(Float 11.0))))

let _ = assert (true = assert_raises (typecheck empty) (Equals(Prim(Int 11), Prim(Float 11.0))))

(* NotEquals *)
let _ = assert ( TBool = typecheck empty (NotEquals(Prim(Int 11), Prim(Int 11))))
let _ = assert ( TBool = typecheck empty (NotEquals(Prim(Float 11.0), Prim(Float 11.0))))

let _ = assert (true = assert_raises (typecheck empty) (NotEquals(Prim(Int 11), Prim(Float 11.0))))

(* Less *)
let _ = assert ( TBool = typecheck empty (Less(Prim(Int 11), Prim(Int 11))))
let _ = assert ( TBool = typecheck empty (Less(Prim(Float 11.0), Prim(Float 11.0))))

let _ = assert (true = assert_raises (typecheck empty) (Less(Prim(Int 11), Prim(Float 11.0))))

(* LessOrEquals *)
let _ = assert ( TBool = typecheck empty (LessOrEquals(Prim(Int 11), Prim(Int 11))))
let _ = assert ( TBool = typecheck empty (LessOrEquals(Prim(Float 11.0), Prim(Float 11.0))))

let _ = assert (true = assert_raises (typecheck empty) (LessOrEquals(Prim(Int 11), Prim(Float 11.0))))

(* Greater *)
let _ = assert ( TBool = typecheck empty (Grater(Prim(Int 11), Prim(Int 11))))
let _ = assert ( TBool = typecheck empty (Grater(Prim(Float 11.0), Prim(Float 11.0))))

let _ = assert (true = assert_raises (typecheck empty) (Grater(Prim(Int 11), Prim(Float 11.0))))

(* GreaterOrEquals *)
let _ = assert ( TBool = typecheck empty (GraterOrEquals(Prim(Int 11), Prim(Int 11))))
let _ = assert ( TBool = typecheck empty (GraterOrEquals(Prim(Float 11.0), Prim(Float 11.0))))

let _ = assert (true = assert_raises (typecheck empty) (GraterOrEquals(Prim(Int 11), Prim(Float 11.0))))

(* Sequence *)
let _ = assert ( TInt = typecheck empty (Sequence(AssignVar("x", (Prim(Int 3))), Add(GetVal "x", Prim(Int 3)))))



(*
let _ = assert ( getFirstVarValue ("myVar",TPrimitive, (Int 5)) = 5 )
let _ = assert ( getFirstVarType ("myVar",TPrimitive, (Int 5)) = TPrimitive )
*)