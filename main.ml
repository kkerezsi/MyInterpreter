open Ast;;
open Interpreter;;
open Typecheck;;
open Printf;;

let myHeap = ref [];;
let myEnv  = ref [];;

(*** Interpreter ***)

(* [interp e] first type checks [e] in the empty 
   environment.  If that succeeds, then [interp]
   evaluates [e] to a value [v], where e -->* v.
   That evaluation should never raise an exception,
   becuase [e] typechecks. *)
let interp e ref env heap =
  (*ignore(typecheck empty e); multistep e*)
  multistep e ref env heap


(* A few test cases *)
let assert_raises f x =
  try ignore(f x); false with
  | _ -> true
  
let _ = assert (Prim (Int 22) = (interp (Prim (Int 22)) ref myEnv myHeap ));;

let _ = print_int (getIntVal (interp (Prim (Int 22)) ref myEnv myHeap));;
printf "\n";;

let _ = assert (Prim (Int 22) = interp (Add(Prim (Int 11), Prim(Int 11))) ref myEnv myHeap)

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
(*
let _ = assert ( getFirstVarValue ("myVar",TPrimitive, (Int 5)) = 5 )
let _ = assert ( getFirstVarType ("myVar",TPrimitive, (Int 5)) = TPrimitive )
*)


let _ = assert (Prim (Int 33) = (interp (Add ( Prim(Int 22) , Prim(Int 11) )) ref myEnv myHeap ));;
let _ = assert (Prim (Int 6) = (interp (Mult ( Prim(Int 2) , Prim(Int 3) )) ref myEnv myHeap ));;
let _ = assert (Prim (Bool true) = (interp (Less ( Prim(Int 1) , Prim(Int 11) )) ref myEnv myHeap ));;
let _ = assert (Prim (Bool true) = (interp (LessOrEquals ( Prim(Int 11) , Prim(Int 11) )) ref myEnv myHeap ));;
let _ = assert (Prim (Bool true) = (interp (LessOrEquals ( Prim(Float 1.0) , Prim(Float 11.0) )) ref myEnv myHeap ));;
let _ = assert (Prim (Bool false) = (interp (LessOrEquals ( Prim(Int 22) , Prim(Int 11) )) ref myEnv myHeap ));;
let _ = assert (Prim (Bool true) = (interp (Equals ( Prim(Int 22) , Prim(Int 22) )) ref myEnv myHeap ));;
let _ = assert (Prim (Bool true) = (interp (NotEquals ( Prim(Int 22) , Prim(Int 2) )) ref myEnv myHeap ));;



let _ = assert (Prim (Int 4) = (interp (
If( (LessOrEquals( Prim(Int 22) , Prim(Int 2))) ,
   	
   	(Equals ( Prim(Int 22) , Prim(Int 22) )),

   	(Prim (Int 4))
							)) ref myEnv myHeap ));;


let _ = assert (Prim (Bool true) = (interp (
If( (LessOrEquals( Prim(Int 2) , Prim(Int 22))) ,
   	
   	(Equals ( Prim(Int 22) , Prim(Int 22) )),

   	(Prim (Int 4))
							)) ref myEnv myHeap ));;



let _ = assert (Prim (Int 9) = (interp (
								Sequence( 
								Add(Prim (Int 1), Prim (Int 1)),
								Add(Prim (Int 4), Prim (Int 5)) )
							   ) ref myEnv myHeap ));;
 

let _ = assert (Prim (Int 4) = (interp (
								Sequence( 
									Add(Prim (Int 1), Prim (Int 1)),
									Sequence( Add(Prim (Int 4), Prim (Int 5)),
											  Add(Prim (Int 2), Prim (Int 2))
								    )
								  )
							   ) ref myEnv myHeap ));;



let _ = assert (Prim (Int 18) = (interp (
								Sequence( Add(Prim (Int 1), Prim (Int 1)),
								Sequence( Sub(Prim (Int 4), Prim (Int 5)),
								Sequence( Mult(Prim (Int 3), Prim (Int 4)),
								Sequence( Add(Prim (Int 3), Prim (Int 5)),
								Sequence( Div(Prim (Int 6), Prim (Int 2)),
								Sequence( Add(Prim (Int 8), Prim (Int 3)),
										  Add(Prim (Int 9), Prim (Int 9))
								    ))))))
							   ) ref myEnv myHeap ));;


let _ = assert (Prim (Int 0) = (interp (AssignVar("x", Prim (Int 3))) ref myEnv myHeap ));;


let _ = assert (Prim (Bool true) = (interp (
	Sequence(
			AssignVar("x", Prim (Int 3)),
			Equals( GetVal("x"), Prim (Int 3) )
		)
) ref myEnv myHeap ));;



let _ = assert (Prim (Bool true) = (interp (
	Sequence( AssignVar("x", Prim (Int 3)),
	Sequence( AssignVar("x", Prim (Int 5)),
		      Equals(GetVal("x"), Prim (Int 5))
		))
) ref myEnv myHeap ));;


let _ = assert (Prim (Bool true) = (interp (
	Sequence( AssignVar("x", Prim (Int 3)),
	Sequence( AssignVar("y", Prim (Int 3)),
		      Equals( GetVal("x"), GetVal("y") )
		))
) ref myEnv myHeap ));;