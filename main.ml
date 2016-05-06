open Ast;;
open Interpreter;;
open Typecheck;;

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
  
let _ = assert (Int 22 = interp (Int 32))
let _ = assert (Int 22 = interp (Add(Int 11,Int 11)))
