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

let get_primitive_val = function
     | Prim x -> x
     | _ -> failwith "Unable to get primitive val"

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


let rec getType v = function
 | []       -> failwith "Empty list"
 | (h::t)   -> 
   match h with
   | (key,TypeVal(mt,_)) -> if(key = v) then mt
                           else getType v t

let getTypeExp = function
  | Prim (Int _ ) | Prim(Float _) | Prim(Bool _) | Prim(Unit) -> TPrimitive
  | Prim (Loc _ )                                               -> TClass
  | _                                                           -> failwith "Could not find type"
                  
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

(* replaces in myList a's value with b*)
let rec replaceInList a b filledUp = function
  | [] -> List.rev filledUp
  | (h::t) -> match h with
              | (name, texp) -> if name = a then
                                replaceInList a b ((name,b)::filledUp) t
                              else
                                replaceInList a b ((name,texp)::filledUp) t

(*string typ e reference environment*)
let updateVar v typV e ref env =
  env := (replaceInList (v) (TypeVal(typV, get_primitive_val e)) [] !env);
  Int 0;;

let defineVar v typV e ref env =
  env :=  (v,TypeVal(typV, get_primitive_val e))::!env;
  Int 0;;
(*------------------------------*)

let updateVarHeap myLoc fldName e ref env =
  env := (replaceInHeap myLoc fldName (TypeVal(typV, get_primitive_val e)) [] !env);
  Int 0;

(*------------God I beg for murcy------------------*)
let rec replaceInHeap my_loc mvar mval filledUp = function
  | [] -> List.rev filledUp
  | (h::t) -> match h with
              | (loc,ObjVal(cName, fldEnv)) -> if ( loc  = my_loc) then  
                                               match fldEnv with 
                                               | FieldEnv(h2::t2) -> 
                                               replaceInHeap my_loc mvar mval ( 
                                                (my_loc ,ObjVal(cName,   FieldEnv(replaceInList mvar mval [] (h2::t2))
                                                                                        ))::filledUp) t
                                               | FieldEnv []               -> failwith "matching went wrong for empty list"
                                              else
                                               replaceInHeap my_loc mvar mval (
                                                (my_loc ,ObjVal(cName,fldEnv))::filledUp) t


(*------------------------------*)


(* -- Environment var operations -- *)

(* -- Heap operations -- *)

let rec isDefinedLoc loc = function
  | []     -> false
  | (h::t) -> match h with
              | (l,_) -> if l = loc then true
                         else isDefinedLoc loc t 

let rec getFieldE loc = function
  | [] -> failwith "loc does not exist"
  | (h::t) -> match h with
              | (l,ObjVal(_,FieldEnv(fieldEnvList) )) -> if l = loc then fieldEnvList
                                          else getFieldE loc t

let rec isDefinedField field = function
  | []     -> false
  | (h::t) -> match h with
              | (f,_) -> if f = field then true
                         else isDefinedField field t

let is_subtype t1 t2 = match (t1,t2) with
   |(TPrimitive, TPrimitive)       -> true
   |(TBot, TBot)                   -> true
   |(TBot, TClass)                 -> true
   |(TClass,TClass)                -> true
   |(_,_)                          -> false

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

let init = function
| TPrimitive  -> Prim(Int 0)
| _           -> failwith "Not implemented yet"

(* -- General purpose functions -- *)  

(*-step implementation,
  -one step at a time*)
let rec step ref env heap = function
  | Prim _                -> failwith "not a step"
  | Var _                 -> failwith "Unbound variable"
  | Init (var,typ,e)      -> step_init var typ e ref env heap 
  | GetVal(var)           -> step_getVal var ref env heap
  | GetField(var,fld)     -> step_getVarFld var fld ref env heap
  | AssignVar(v,e)        -> step_assign v e ref env heap
  | AssignField(v,fld,e)  -> step_assign_fld v fld e ref env heap 
  | Add(e1, e2)           -> step_add e1 e2 ref env heap
  | Sub(e1, e2)           -> step_sub e1 e2 ref env heap
  | And(e1, e2)           -> step_and e1 e2 ref env heap
  | If(e1,e2,e3)          -> step_if e1 e2 e3 ref env heap
  | Mult(e1,e2)           -> step_mult e1 e2 ref env heap
  | Div(e1,e2)            -> step_div e1 e2 ref env heap
  | Or(e1,e2)             -> step_or e1 e2 ref env heap
  | Not(e)                -> step_not e ref env heap
  | Equals(e1,e2)         -> step_eql e1 e2 ref env heap
  | NotEquals(e1,e2)      -> step_neql e1 e2 ref env heap
  | Less(e1,e2)           -> step_less e1 e2 ref env heap
  | LessOrEquals(e1,e2)   -> step_lessEql e1 e2 ref env heap
  | Grater(e1,e2)         -> step_grater e1 e2 ref env heap
  | GraterOrEquals(e1,e2) -> step_graterEql e1 e2 ref env heap
  | While(e1,e2)          -> step_while e1 e2 ref env heap
  | Sequence(e1,e2)       -> step_seq e1 e2 ref env heap
  | BlockWithoutVar(e1)         -> step_block_nvar e1 ref env heap
  | BlockWithVar(myTyp,name,e1) -> step_block_var myTyp name e1 ref env heap
  | Ret(v,e)                    -> step_ret v e ref env heap
  | New(cName,lexpr)            -> step_new cName lexpr ref env heap 
  | Call(cName,mName,lexpr)     -> step_call cName mName lexpr ref env heap 
 

and 
  step_init var v_typ e ref env heap = 
    if ( (Int 0) = defineVar var v_typ e ref env ) then 
      Ret(var, e)
    else
      failwith "Unable to step return"

and 
  step_getVal var ref env heap = 
    if(isDefinedVar var !env) then
       getVarVal var !env 
    else
      failwith "Error ! Var name does not exist in the environment"

and 
  step_getVarFld var fld ref env heap = 
    if (isDefinedVar var !env) then
        let loc = getVarVal var !env in 
          if(isLocation loc) && (isDefinedLoc loc !heap) then
            let fldE = getFieldE loc !heap in
              if (isDefinedField fld fldE) then 
                let fldVal = getVarVal fld fldE in
                  fldVal
              else
                failwith "Error !Fild env could not be found"
          else 
            failwith "Error! Loc could not be found"
    else
      failwith "Error !Var name does not exist in the environment"
and 
  step_new cName lexpr ref env heap = Prim(Int 10)

and 
  step_call cName mName lexpr ref env heap = Prim(Int 10)

and
  step_assign v e ref env heap =

    if is_primitive_val e then 
      if isDefinedVar v !env then
       let typ_var = getType v !env in
       let typ_val = getTypeExp e   in
        if is_subtype typ_val typ_var then
          Prim (updateVar v typ_val e ref env )
        else failwith "Error! not subtypes"
      else 
        let typ_val = getTypeExp e   in
        if is_subtype typ_val typ_val then
          Prim (defineVar v typ_val e ref env)
        else failwith "Error! not subtypes"
    else AssignVar(v ,step ref env heap e)
and  
  step_assign_fld v fld e ref env heap = 
  if is_primitive_val e then 
    if (isDefinedVar v !env) then
        let loc = getVarVal v !env in 
          if(isLocation loc) && (isDefinedLoc loc !heap) then
            let fldE = getFieldE loc !heap in
              if isDefinedField fld fldE then 
                let fldVal = getVarVal fld fldE in
                let typ_val = getTypeExp e in
                let typ_fld = getTypeExp fldVal in 
                if(is_subtype typ_val typ_fld) then
                (*Need impl*)
                  Prim(Unit)
                else
                  failwith "Error !Fild env could not be found"
              else
                failwith "Error !Fild env could not be found"
          else 
            failwith "Error! Loc could not be found"
    else
      failwith "Error !Var name does not exist in the environment"
  else
    AssignField(v,fld,step ref env heap e)

and
  step_ret v e ref env heap =  
    if is_primitive_val e then 
      Prim ( get_primitive_val e )
    else
        Ret(v,step ref env heap e)

and
  (* Eval e1, eval e2, add the values *)
  step_add e1 e2 ref env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Int i),Prim (Int j)) -> Prim(Int (i+j))
      | (Prim (Float i),Prim (Float j)) -> Prim(Float (i+.j))
      | _ -> failwith "Run-time type error: add"
    else Add(e1, step ref env heap e2)
  else Add(step ref env heap e1, e2)

and
  (* Eval e1, eval e2, add the values *)
  step_sub e1 e2 ref env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Int i),Prim (Int j)) -> Prim(Int (i-j))
      | (Prim (Float i),Prim (Float j)) -> Prim(Float (i-.j))
      | _ -> failwith "Run-time type error: sub"
    else Sub(e1, step ref env heap e2)
  else Sub(step ref env heap e1, e2)

and
  
  (* Eval e1, eval e2, multiply the values *)
  step_mult e1 e2 ref env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match(e1,e2) with
      | (Prim (Int i),Prim (Int j)) -> Prim(Int (i*j))
      | (Prim (Float i),Prim (Float j)) -> Prim(Float (i*.j))
      | _ -> failwith "Run-time type error: mult"
    else Mult(e1, step ref env heap e2)
  else Mult(step ref env heap e1, e2)

and 

  step_div e1 e2 ref env heap = 
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
    | (Prim (Int i),Prim (Int j)) -> Prim(Int (i/j))
    | (Prim (Float i),Prim (Float j)) -> Prim(Float (i/.j))
    | _ -> failwith "Run-time type error: div"
    else Div(e1, step ref env heap e2)
  else Div(step ref env heap e1, e2)

and
  (* Eval e1, eval e2, && the values *)
  step_and e1 e2 ref env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Bool x),Prim (Bool y)) -> Prim(Bool (x&&y))
      | _ -> failwith "Run-time type error: and"
    else And(e1, step ref env heap e2)
  else And(step ref env heap e1, e2)

and

  step_if e1 e2 e3 ref env heap =
  if is_primitive_val e1 then
    match e1 with
      | Prim(Bool true) -> e2
      | Prim(Bool false) -> e3
      | _ -> failwith "Run-time type error (if)"
  else If(step ref env heap e1, e2, e3)

and
  step_or e1 e2 ref env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Bool x),Prim (Bool y)) -> Prim(Bool (x||y))
      | _ -> failwith "Run-time type error: or"
    else And(e1, step ref env heap e2)
  else And(step ref env heap e1, e2)

and

  step_not e ref env heap =
  if is_primitive_val e  
  then match e with
    | Prim(Bool x) -> Prim(Bool (not x))
    | _ -> failwith "Run-time type error: not"
  else Not(step ref env heap e)

(*------CMP---------*)

and
  step_eql e1 e2 ref env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Int x),Prim (Int y)) -> Prim(Bool (x = y))
      | (Prim (Float x),Prim (Float y)) -> Prim(Bool (x = y))
      | _ -> failwith "Run-time type error: eql"
    else Equals(e1, step ref env heap e2)
  else Equals(step ref env heap e1, e2)

and
  step_neql e1 e2 ref env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Int x),Prim (Int y)) -> Prim(Bool (x <> y))
      | (Prim (Float x),Prim (Float y)) -> Prim(Bool (x <> y))
      | _ -> failwith "Run-time type error: neql"
    else NotEquals(e1, step ref env heap e2)
  else NotEquals(step ref env heap e1, e2)

  and
  step_less e1 e2 ref env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Int x),Prim (Int y)) -> Prim(Bool (x < y))
      | (Prim (Float x),Prim (Float y)) -> Prim(Bool (x < y))
      | _ -> failwith "Run-time type error: <"
    else Less(e1, step ref env heap e2)
  else Less(step ref env heap e1, e2)

  and
  step_lessEql e1 e2 ref env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Int x),Prim (Int y)) -> Prim(Bool (x <= y))
      | (Prim (Float x),Prim (Float y)) -> Prim(Bool (x <= y))
      | _ -> failwith "Run-time type error: <="
    else Less(e1, step ref env heap e2)
  else Less(step ref env heap e1, e2)

  and
  step_grater e1 e2 ref env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Int x),Prim (Int y)) -> Prim(Bool (x > y))
      | (Prim (Float x),Prim (Float y)) -> Prim(Bool (x > y))
      | _ -> failwith "Run-time type error: and"
    else Grater(e1, step ref env heap e2)
  else Grater(step ref env heap e1, e2)

  and
  step_graterEql e1 e2 ref env heap =
  if is_primitive_val e1
  then if is_primitive_val e2
    then match (e1,e2) with
      | (Prim (Int x),Prim (Int y)) -> Prim(Bool (x >= y))
      | (Prim (Float x),Prim (Float y)) -> Prim(Bool (x >= y))
      | _ -> failwith "Run-time type error: >="
    else GraterOrEquals(e1, step ref env heap e2)
  else GraterOrEquals(step ref env heap e1, e2)

(*------CMP---------*)
and
  step_while e1 e2 ref env heap =
  if is_primitive_val e1
  then  match  e1 with
    | Prim(Bool true) -> While(e1, step ref env heap e2)
    | Prim(Bool false) -> e2
    | _-> failwith "Run-time type error: while"
  else While(step ref env heap e1, e2)


and
  step_seq e1 e2 ref env heap = 
    if is_primitive_val e1 then
      if is_primitive_val e2 then
        Prim (get_primitive_val e2)
      else Sequence ( e1,step ref env heap e2)
    else Sequence ( step ref env heap e1, e2)

and 

  step_block_nvar e1 ref env heap = 
    if is_primitive_val e1 then
      e1
    else BlockWithoutVar ( step ref env heap e1)

and 

  step_block_var typ name e1 ref env heap =
    if is_primitive_val e1 then
      e1
    else BlockWithVar( typ, name, step ref env heap e1)


(*multistep*)
let rec multistep e ref env heap =
  if is_primitive_val e then e
  else multistep (step ref env heap e) ref env heap