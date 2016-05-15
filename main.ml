open Ast;;
open Interpreter;;
open Typecheck;;
open Printf;;

let myHeap = ref [];;
let myEnv =  ref [];;

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