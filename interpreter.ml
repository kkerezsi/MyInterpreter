open Ast;;

let is_value = function
  | Int _ | Bool _ | Float _ | Loc _ | Unit -> true
  | _ -> false

let isLocation = function
  | Prim (Loc _) -> true
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
  | [] -> failwith "Empty stack"
  | (h::t) -> match h with
    | (i,_) -> i

let getFirstVarValue = function
  | [] -> failwith "Empty stack"
  | (h::t) -> match h with
    | (_, TypeVal (_,j)) -> j
      

let popFirstVar = function
  | [] -> failwith "Empty stack"
  | (h::t) -> t


let getVal a stk = 
 try List.assoc a stk with
  | Not_found -> failwith "Type error (unbound variable)"


let rec isDefinedVar var = function
 | []     -> false
 | (h::t) -> match h with
             | (tv,_) -> 
                          if tv = var then true 
                          else isDefinedVar var t

let rec getVarVal var = function
 | []     -> failwith "Unable to find var value"
 | (h::t) -> match h with
             | (tv,TypeVal(_,myVal) ) ->  if tv = var then Prim(myVal) 
                                          else getVarVal var t


  (* if (isDefinedVar var V) && (loc = getVal var V) &&
     (isLocation loc) && ( isDefinedHeap loc ) &&
     (fldE = getFieldE loc H) && (isDefinedVar fld fldE) &&
     (val = getVal fld fldE) then *)
 



(* -- Environment var operations -- *)

(* -- Heap operations -- *)

let rec isDefinedLoc loc = function
  | [] -> false
  | (h::t) -> match h with
              | (l,_) -> if l = loc then true
                         else isDefinedLoc loc t

let rec getFieldE loc = function
  | [] -> failwith "loc does not exist"
  | (h::t) -> match h with
              | (l,ObjVal(_,FieldEnv(fieldEnvList) )) -> if l = loc then fieldEnvList
                                          else getFieldE loc t

let rec isDefinedField field = function
  | [] -> false
  | (h::t) -> match h with
              | (f,_) -> if f = field then true
                         else isDefinedField field t


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
let rec step env heap = function
  | Prim _          -> failwith "not a step"
  | Var _           -> failwith "Unbound variable"
  | GetVal(var)     -> step_getVal var env heap
  | GetField(var,fld) -> step_getVarFld var fld env heap
  | AssignVar(v,e)  -> step_assign v e env heap 
  | Add(e1, e2)     -> step_add e1 e2 env heap
  | Sub(e1, e2)     -> step_sub e1 e2 env heap
  | And(e1, e2)     -> step_and e1 e2 env heap
  | If(e1,e2,e3)    -> step_if e1 e2 e3 env heap
  | Mult(e1,e2)     -> step_mult e1 e2 env heap
  | Div(e1,e2)      -> step_div e1 e2 env heap
  | Or(e1,e2)       -> step_or e1 e2 env heap
  | Not(e)          -> step_not e env heap
  | Equals(e1,e2)   -> step_eql e1 e2 env heap
  | NotEquals(e1,e2)-> step_neql e1 e2 env heap
  | Less(e1,e2)     -> step_less e1 e2 env heap
  | LessOrEquals(e1,e2) -> step_lessEql e1 e2 env heap
  | Grater(e1,e2)   -> step_grater e1 e2 env heap
  | GraterOrEquals(e1,e2) -> step_graterEql e1 e2 env heap
  | While(e1,e2)    -> step_while e1 e2 env heap
  | Sequence(e1,e2) -> step_seq e1 e2 env heap
  | BlockWithoutVar(e1)         -> step_block_nvar e1 env heap
  | BlockWithVar(myTyp,name,e1) -> step_block_var myTyp name e1 env heap
  | Ret(v,e)                    -> step_ret v e env heap
  | New(cName,lexpr)            -> step_new cName lexpr env heap 
  | Call(cName,mName,lexpr)     -> step_call cName mName lexpr env heap 
  | _ -> failwith "Run-time type error: unknown command"
 

and 
  step_getVal var env heap = 
    if(isDefinedVar var env) then
       getVarVal var env 
    else
      failwith "Error ! Var name does not exist in the environment"

and 
  step_getVarFld var fld env heap = 
    if (isDefinedVar var env) then
        let loc = getVarVal var env in 
          if(isLocation loc) && (isDefinedLoc loc heap) then
            let fldE = getFieldE loc heap in
              if( isDefinedField fld fldE ) then 
                let fldVal = getVarVal fld fldE in
                  fldVal
              else
                failwith "Error !Fild env could not be found"
          else 
            failwith "Error! Loc could not be found"
    else
      failwith "Error !Var name does not exist in the environment"

and 
  step_new cName lexpr env heap = Prim(Int 10)

and 
  step_call cName mName lexpr env heap = Prim(Int 10)

and
  step_assign v e env heap = Prim( Int 10)
    
and  
  step_ret v e env heap =  Prim (Int 10)

and
  (* Eval e1, eval e2, add the values *)
  step_add e1 e2 env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Int i),Prim (Int j)) -> Prim(Int (i+j))
      | (Prim (Float i),Prim (Float j)) -> Prim(Float (i+.j))
      | _ -> failwith "Run-time type error: add"
    else Add(e1, step env heap e2)
  else Add(step env heap e1, e2)

and
  (* Eval e1, eval e2, add the values *)
  step_sub e1 e2 env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Int i),Prim (Int j)) -> Prim(Int (i-j))
      | (Prim (Float i),Prim (Float j)) -> Prim(Float (i-.j))
      | _ -> failwith "Run-time type error: add"
    else Sub(e1, step env heap e2)
  else Sub(step  env heap e1, e2)

and
  
  (* Eval e1, eval e2, multiply the values *)
  step_mult e1 e2 env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match(e1,e2) with
      | (Prim (Int i),Prim (Int j)) -> Prim(Int (i*j))
      | (Prim (Float i),Prim (Float j)) -> Prim(Float (i*.j))
      | _ -> failwith "Run-time type error: mult"
    else Mult(e1, step env heap e2)
  else Mult(step env heap e1, e2)

and 

  step_div e1 e2 env heap = 
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
    | (Prim (Int i),Prim (Int j)) -> Prim(Int (i/j))
    | (Prim (Float i),Prim (Float j)) -> Prim(Float (i/.j))
    | _ -> failwith "Run-time type error: div"
    else Div(e1, step env heap e2)
  else Div(step env heap e1, e2)

and
  (* Eval e1, eval e2, && the values *)
  step_and e1 e2 env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Bool x),Prim (Bool y)) -> Prim(Bool (x&&y))
      | _ -> failwith "Run-time type error: and"
    else And(e1, step env heap e2)
  else And(step env heap e1, e2)

and

  step_if e1 e2 e3 env heap =
  if is_primitive_val e1 then
    match e1 with
      | Prim(Bool true) -> e2
      | Prim(Bool false) -> e3
      | _ -> failwith "Run-time type error (if)"
  else If(step env heap e1, e2, e3)

and
  step_or e1 e2 env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Bool x),Prim (Bool y)) -> Prim(Bool (x||y))
      | _ -> failwith "Run-time type error: or"
    else And(e1, step  env heap e2)
  else And(step env heap e1, e2)

and

  step_not e env heap =
  if is_primitive_val e  
  then match e with
    | Prim(Bool x) -> Prim(Bool (not x))
    | _ -> failwith "Run-time type error: not"
  else Not(step env heap e)

(*------CMP---------*)

and
  step_eql e1 e2 env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Bool x),Prim (Bool y)) -> Prim(Bool (x = y))
      | _ -> failwith "Run-time type error: and"
    else Equals(e1, step env heap e2)
  else Equals(step env heap e1, e2)

and
  step_neql e1 e2 env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Bool x),Prim (Bool y)) -> Prim(Bool (x <> y))
      | _ -> failwith "Run-time type error: and"
    else NotEquals(e1, step env heap e2)
  else NotEquals(step env heap e1, e2)

  and
  step_less e1 e2 env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Bool x),Prim (Bool y)) -> Prim(Bool (x < y))
      | _ -> failwith "Run-time type error: and"
    else Less(e1, step env heap e2)
  else Less(step env heap e1, e2)

  and
  step_lessEql e1 e2 env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Bool x),Prim (Bool y)) -> Prim(Bool (x <= y))
      | _ -> failwith "Run-time type error: and"
    else Less(e1, step env heap e2)
  else Less(step env heap e1, e2)

  and
  step_grater e1 e2 env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Bool x),Prim (Bool y)) -> Prim(Bool (x > y))
      | _ -> failwith "Run-time type error: and"
    else Grater(e1, step env heap e2)
  else Grater(step env heap e1, e2)

  and
  step_graterEql e1 e2 env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Bool x),Prim (Bool y)) -> Prim(Bool (x >= y))
      | _ -> failwith "Run-time type error: and"
    else GraterOrEquals(e1, step env heap e2)
  else GraterOrEquals(step env heap e1, e2)

(*------CMP---------*)
and
  step_while e1 e2 env heap =
  if is_primitive_val e1
  then  match  e1 with
    | Prim(Bool true) -> While(e1, step env heap e2)
    | Prim(Bool false) -> e2
    | _-> failwith "Run-time type error: while"
  else While(step env heap e1, e2)


and
  step_seq e1 e2 env heap = 
    if is_primitive_val e1 then
      if is_primitive_val e2 then
        Prim Unit
      else Sequence ( e1,step env heap e2)
    else Sequence ( step env heap e1, e2)

and 

  step_block_nvar e1 env heap = 
    if is_primitive_val e1 then
      e1
    else BlockWithoutVar ( step env heap e1)

and 

  step_block_var typ name e1 env heap =
    if is_primitive_val e1 then
      e1
    else BlockWithVar( typ, name, step env heap e1)


(*multistep*)
let rec multistep e env heap =
  if is_primitive_val e then e
  else multistep (step env heap e) env heap