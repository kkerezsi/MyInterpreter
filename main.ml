open Ast;;
open Interpreter;;
open Typecheck;;
open Printf;;

(*** Interpreter ***)

(* [interp e] first type checks [e] in the empty 
   environment.  If that succeeds, then [interp]
   evaluates [e] to a value [v], where e -->* v.
   That evaluation should never raise an exception,
   becuase [e] typechecks. *)
let interp e =
  (*ignore(typecheck empty e); multistep e*)
  multistep e


(* A few test cases *)
let assert_raises f x =
  try ignore(f x); false with
  | _ -> true
  
let _ = assert (Prim (Int 22) = interp ( Prim (Int 22) ))

let _ = print_int (getIntVal (interp(Prim (Int 22))));;
printf "\n";;
let _ = assert (Prim (Int 22) = interp (Add(Prim (Int 11), Prim(Int 11) )))

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
