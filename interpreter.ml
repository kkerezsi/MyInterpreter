open Ast;;

let myHeap = []

let is_value = function
  | Int _ | Bool _ | Float _ | Loc _ | Unit -> true
  | _ -> false

let is_typ = function
  | TPrimitive | TClass | TBot  -> true

let is_primitive = function
  | Prim _         -> true 
  | _              -> false

let is_primitive_val = function
     | Prim (Int _) | Prim(Bool _) | Prim(Float _) | Prim(Loc _) -> true
     | _ -> false

(* -- Stack -- *)
let getFirstStack = function
  | []        -> failwith "Empty stack"
  | (h::t)    -> h


let popFirstStack = function
  | []        -> failwith "Empty stack"
  | (h::t)    -> t


let addStack myList = function
  | x         -> x::myList;;


let rec printList = function
  | [] -> print_int
  | (h::t) -> print_int h; print_string " " ; printList t;;

(* -- Stack -- *)

(* -- Environment var operations -- *)


let getFirstVarName = function
  | VarEnv (x,y) ->  x

let getFirstVarValue = function
  | VarEnv (_,i) ->
    match i with
     | []     -> failwith "Empty stack"
     | (h::t) -> match h with
       | TypeVal(_,i) -> i

let popFirstVar = function
  | VarEnv (x,i) ->
    match i with
     | []     ->  failwith "Empty stack"
     | (h::t) ->  VarEnv (x,t)


(* -- Environment var operations -- *)

(* -- Heap operations -- *)



(* -- Heap operations -- *)

(* -- General purpose functions -- *)  
let getIntVal x = 
  if is_primitive x
    then match x with
    | (Prim (Int y)) ->  y
    | _ -> failwith "not found"
    else
      failwith "not found";;

let getBoolVal x = 
  if is_primitive x
    then match x with
    | (Prim (Bool y)) ->  y
    | _ -> failwith "not found"
    else
      failwith "not found";;


let getFloatVal x = 
  if is_primitive x
    then match x with
    | (Prim (Float y)) ->  y
    | _ -> failwith "not found"
    else
      failwith "not found";;

let getLocVal x = 
  if is_primitive x
    then match x with
    | (Prim (Loc y)) ->  y
    | _ -> failwith "not found"
    else
      failwith "not found";;

(* -- General purpose functions -- *)  

(*-step implementation,
  -one step at a time*)
let rec step = function
  | Prim _ -> failwith "not a step"
  | Var _ -> failwith "Unbound variable"
  | Add(e1, e2) -> step_add e1 e2
  | Sub(e1, e2) -> step_sub e1 e2
  | And(e1, e2) -> step_and e1 e2
  | If(e1,e2,e3) -> step_if e1 e2 e3
  | Mult(e1,e2) -> step_mult e1 e2
  | Div(e1,e2) -> step_div e1 e2
  | Or(e1,e2)  -> step_or e1 e2
  | Not(e) -> step_not e
  | While(e1,e2) -> step_while e1 e2
  | Sequence(e1,e2) -> step_seq e1 e2 
  | BlockWithoutVar(e1) -> step_block_nvar e1
  | BlockWithVar(myTyp,name,e1) -> step_block_var myTyp name e1
  (*| New(st,li) -> step_new st li
    | Call(st1,st2) -> step_call st1 st2  ///to be implemented*)
  | _ -> failwith "Run-time type error: unknown command"
  
 
and
  (* Eval e1, eval e2, add the values *)
  step_add e1 e2 =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Int i),Prim (Int j)) -> Prim(Int (i+j))
      | (Prim (Float i),Prim (Float j)) -> Prim(Float (i+.j))
      | _ -> failwith "Run-time type error: add"
    else Add(e1, step e2)
  else Add(step e1, e2)

and
  (* Eval e1, eval e2, add the values *)
  step_sub e1 e2 =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Int i),Prim (Int j)) -> Prim(Int (i-j))
      | (Prim (Float i),Prim (Float j)) -> Prim(Float (i-.j))
      | _ -> failwith "Run-time type error: add"
    else Sub(e1, step e2)
  else Sub(step e1, e2)

and
  
  (* Eval e1, eval e2, multiply the values *)
  step_mult e1 e2 =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match(e1,e2) with
      | (Prim (Int i),Prim (Int j)) -> Prim(Int (i*j))
      | (Prim (Float i),Prim (Float j)) -> Prim(Float (i*.j))
      | _ -> failwith "Run-time type error: mult"
    else Mult(e1, step e2)
  else Mult(step e1, e2)

and 

  step_div e1 e2 = 
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
    | (Prim (Int i),Prim (Int j)) -> Prim(Int (i/j))
    | (Prim (Float i),Prim (Float j)) -> Prim(Float (i/.j))
    | _ -> failwith "Run-time type error: div"
    else Div(e1, step e2)
  else Div(step e1, e2)

and
  (* Eval e1, eval e2, && the values *)
  step_and e1 e2 =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Bool x),Prim (Bool y)) -> Prim(Bool (x&&y))
      | _ -> failwith "Run-time type error: and"
    else And(e1, step e2)
  else And(step e1, e2)

and

  step_if e1 e2 e3 =
  if is_primitive_val e1 then
    match e1 with
      | Prim(Bool true) -> e2
      | Prim(Bool false) -> e3
      | _ -> failwith "Run-time type error (if)"
  else If(step e1, e2, e3)

and
  step_or e1 e2 =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Bool x),Prim (Bool y)) -> Prim(Bool (x||y))
      | _ -> failwith "Run-time type error: or"
    else And(e1, step e2)
  else And(step e1, e2)

and

  step_not e =
  if is_primitive_val e  
  then match e with
    | Prim(Bool x) -> Prim(Bool (not x))
    | _ -> failwith "Run-time type error: not"
  else Not(step e)

and
  step_while e1 e2 =
  if is_primitive_val e1
  then  match  e1 with
    | Prim(Bool true) -> While(e1, step e2)
    | Prim(Bool false) -> e2
    | _-> failwith "Run-time type error: while"
  else While(step e1, e2)


and
  step_seq e1 e2 = 
    if is_primitive_val e1 then
      if is_primitive_val e2 then
        Prim Unit
      else Sequence ( e1,step e2)
    else Sequence ( step e1, e2)

and 

  step_block_nvar e1 = 
    if is_primitive_val e1 then
      e1
    else BlockWithoutVar ( step e1 )

and 

  step_block_var typ name e1 =
    if is_primitive_val e1 then
      e1
    else BlockWithVar( typ, name, step e1)


(*multistep*)
let rec multistep e =
  if is_primitive_val e then e
  else multistep (step e)

(*
(*lookup*)
let lookup ctx x =
  try List.assoc x ctx with
    | Not_found -> failwith "Type error (unbound variable)"

(*extend*)
let extend ctx x t =
  (x,t)::ctx
*)

