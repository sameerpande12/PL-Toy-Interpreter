(* (key  ,leftchild ,rightchild, parent)  or empty node*)
exception InvalidParameterNumbers
exception InvalidProcedureName
exception EmptyStackException
exception InvalidStackException
exception ElementNotFoundException
type funcNode = Node of (string * (string list) * (string list) * funcNode) | Empty
(*funciton name , parameters list, local variables list*)
type answer = N of int | NULL

type identity = ID of (string * (string list) *(string list))
let main = Node("main",[],["a";"b";"c"],Empty)
and p =Node("P",["x";"y"],["z";"a"],main)
and q =Node("Q",["z";"w"],["x","b"],main)
and r =Node("R",["w";"i"],["j","b"],p)
and s =Node("S",["c";"k"],["m","n"],p)
and t =Node("T",["a";"y"],["i","f"],q)
and u =Node("U",["c";"z"],["p","g"],q)
and v =Node("V",["m";"n"],["c"],r)
and w =Node("W",["m";"p"],["j","h"],t)



let rec getID x = match x with
    main ->ID("main",[],["a";"b";"c"])
  | p -> ID("P",["x";"y"],["z";"a"])
  | q -> ID("Q",["z";"w"],["x","b"])
  | r -> ID("R",["w";"i"],["j","b"])
  | s -> ID("S",["c";"k"],["m","n"])
  | t -> ID("T",["a";"y"],["i","f"])
  | u -> ID("U",["c";"z"],["p","g"])
  | v -> ID("V",["m";"n"],["c"])
  | w -> ID("W",["m";"p"],["j","h"])
  | _ -> raise InvalidProcedureName

let getVariablesUsed fnode = match fnode with
    (_,parameters,locals,_)-> parameters @ locals



let getProcedure x = match x with
    "main" -> main
  | "P"->p
  | "Q"->q
  | "R"->r
  | "S"->s
  | "T"->t
  | "U"->u
  | "V"->v
  | "W"->w
  | _ -> raise InvalidProcedureName

let registers = [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0] in

let rec getAllAncestors fnode =
  match fnode with
    Empty -> []
  | Node(_,_,_,pnode) -> [fnode]::(getAllAncestors pnode)

let getParent fnode = match fnode with
    Node(_,_,_,x)->x

let rec isMember a mlist = match mlist with
    [] -> false
  | m::mlist1 -> if(a=m)then true else isMember a mlist1

let rec union l1 l2 = match l1 with
    [] -> l2
  | l::list1 -> if(isMember l l2)then (union list1 l2 )
    else union list1 (l::l2)


let canCall pnode qnode = (*asks if pnode can call qnode*)
  let ancestors = getAllAncestors pnode in
  if (isMember qnode ancestors) || (isMember (getParent qnode) ancestors) then true
  else false

let rec generateNull n =
  if(n = 0)then []
  else [NULL]::(generateNull (n-1))

let getLocalVariablesSize x = match x with
    Node(_,_,l,_)-> List.length l
let getLocals x = match x with
    Node(_,_,l,_) -> l

let getParametersSize x = match x with
    Node(_,para,_,_)-> List.length para


let rec getNodesTillParent p nlist = match nlist with (*returns list till you find p in the list *)
    [] -> []
  | n::nlist1 -> if ( (getParent p) = n) then [n]
    else n::(getNodesTillParent p nlist1)

let rec getNodesBeforeParent p nlist = match nlist with (*returns list till you find p in the list *)
    [] -> []
  | n::nlist1 -> if ( (getParent p) = n) then []
    else n::(getNodesBeforeParent p nlist1)


let rec removeNodesBeforeParent p nlist = match nlist with
    []->[]
  |n::nlist1 -> if (getParent p) = n then [] else removeNodesBeforeParent nlist1

let getFrameSize fnode = match fnode with
    Node(name,parameters,locals,_)-> 4 + (List.length parameters)+ (List.length locals)

let callProcedure pname parameters stack callstack fp =  (*returns a tuple of modified stack , call stack, fp*)
  let callee = getProcedure pname in
  let caller = List.hd callstack
  if canCall caller (callee) then
    let toMoveFromFpDL =  2 + (List.length parameters) + (getLocalVariablesSize (List.hd callstack)) + 1 + 1 in

    let toMoveFromFpSL =
          let nodesTillParent = getNodesTillParent callee callstack in
                  (2 + getParametersSize callee)  +  (List.map getFrameSize nodesTillParent) - (2 + getParametersSize (List.hd nodesTillParent))
    in
    ( (generateNull (getLocalVariablesSize callee))@registers@(getID callee)@(N (toMoveFromFpDL))@(N(toMoveFromFpSL))@parameters@stack , callee::callstack, fp + toMoveFromFpDL)
                                                    (* FP *)
  else
    raise InvalidFunctionCall


let rec removeBeforeId stack = match stack with
    [] -> []
  | ID(_,_,_)::stack1 -> stack
  | s::stack1 -> removeBeforeId stack1

let rec getBeforeId stack = match stack with
    [] -> []
  | ID(_,_,_)::stack1 -> []
  | s::stack1 -> s :: (getBeforeId stack1)


let rec createAssociation alist blist = match (alist,blist) with
    ([],[])-> []
  | ([],list1)-> raise IncompatibleAssociation
  | (list1,[])->raise IncompatibleAssociation
  | (a::a1,b::b1)->(a,b)::(createAssociation a1 b1)

let rec numElementsTillId stack  = match stack with(*excluding the ID*)
    [] -> 0
  | ID(_,_,_)::stack1 -> 0
  | s::stack1 -> 1 + numElementsTillId stack1

let rec removeFirstN n stack =
  if n <= 0 then stack
  else match stack with
      [] -> raise EmptyStackException
    | s::s1 -> removeFirstN (n-1) s1

let rec getFirstN n stack =
  if(n <= 0)then []
  else (List.hd stack)::(getFirstN (n-1) (List.tl stack))

let rec isKeyIn x l = match l with
    [] -> false
  | ((s,t)::l1) -> if (s = x) && (t <> NULL) then true else isKeyIn x l1

let rec unionWithOverWrite l1 l2 = match l1 with (*union of l2 and l1 keys preferring value of l2*)
    [] -> l2
  | ((k1,v1)::list1) -> if isKey k1 l2 then unionWithOverWrite list1 l2
    else (List.hd l1)::(unionWithOverWrite list1 l2)

let  currentVariableValuePairs stack callstack =
  let topFunction = List.hd callstack in
  let localAndParameters = match topFunction with
      Node(_,parameters,locals,parent) ->(locals,parameters) in

  let parameterValues = getFirstN (getParametersSize topFunction) (removeFirstN 3 (removeBeforeId stack)) in
  let parameterVariables = snd localAndParameters in

  let elementsTillId = numElementsTillId stack in
  let localValues = removeFirstN (elementsTillId -1 - (getLocalVariablesSize topFunction)) (getFirstN (elementsTillId - 1 ) stack) in
  let localVariables = fst localAndParameters in

  let localvariableValuePairs = createAssociation localVariables localValues in
  let parametervariableValuePairs = createAssociation parameterVariables parameterValues in

  let variableValueLists = unionWithOverWrite parametervariableValuePairs localvariableValuePairs

let moveToStaticParentLevel stack callstack fp =
  if(List.tl callstack = [])then (stack,callstack,fp)
  else
    let toMoveFromFpSL = match   List.hd(List.tl (List.tl (removeBeforeId stack))) with N(x)-> x | _ -> raise InvalidStackException in

    let topProcedure = List.hd callstack in
    let modifiedStack =   (removeBeforeId stack) in
    let numLocalsInStaticParent = match List.hd(removeFirstN toMoveFromFpSL modifiedStack) with
        ID(x,parameters,locals)->List.length locals
      | _ -> raise InvalidStackException in

    let newCallStack = removeNodesBeforeParent (List.hd callstack) callstack in (*keeps the parent*)
    let modifiedStack = (removeFirstN (toMoveFromFpSL -1 - getLocalVariablesSize(List.hd newCallStack)) modifiedStack) in
    (modifiedStack,newCallStack,fp - toMoveFromFpSL)

let getStacksBeforeStaticParent  stack callstack =
  if(List.tl callstack = []) then (stack,callstack)
  else
   let toMoveFromFpSL = match   List.hd(List.tl (List.tl (removeBeforeId stack))) with N(x)-> x | _ -> raise InvalidStackException in
   let topProcedure = List.hd callstack in
   let modifiedStack =   (removeBeforeId stack) in
   let numLocalsInStaticParent = match List.hd(removeFirstN toMoveFromFpSL modifiedStack) with
       ID(x,parameters,locals)->List.length locals
     | _ -> raise InvalidStackException in
   let newCallStack = getNodesBeforeParent (List.hd callstack) callstack in (*keeps the parent*)
   let modifiedStack = (getBeforeId stack)@(getFirstN (toMoveFromFpSL -1 - getLocalVariablesSize(List.hd newCallStack)) modifiedStack) in

   (modifiedStack,newCallStack)



let rec getAllAccessibleVariables (stack, callstack, fp) =
  if(List.tl callstack = [])then (currentVariableValuePairs stack callstack)
  else
    unionWithOverWrite (getAllAccessibleVariables(moveToStaticParentLevel stack callstack fp))  (currentVariableValuePairs stack callstack)

let returnBack stack callstack fp=  (*gives current frame pointer in fp*)
  let topProcedure = List.hd callstack in
  let modifiedStack =  List.removeFirstN (3 + List.length (getParametersSize topProcedure)) (removeBeforeId stack) in
  let toMoveFromFpDL = match List.hd(List.tl (removeBeforeId stack)) with
      N(x)-> x
    |_ -> raise InvalidStackException  in
  (modifiedStack,  List.tl callstack  , fp - toMoveFromFpDL)


let rec getPosition x mlist = match x with
    [] -> raise ElementNotFoundException
  | m::n -> if(m = x )then 1 else 1 + getPosition

let rec modifyVariable x value (stack,callstack,fp) =
  if isMember x getLocals(List.hd callstack) then
    let rankInLocals = getPosition x getLocals(List.hd callstack)in
    let elementsBeforeVariable = numElementsTillId stack -1 -getLocalVariablesSize (List.hd callstack) + rankInLocals - 1in
    (getFirstN elementsBeforeVariable stack )@(N(value))@ (removeFirstN (elementsBeforeVariable+1)  stack)
  else

    let (stack1,callstack1) =  getStacksBeforeStaticParent stack callstack in
    let (stack2,callstack2) =  (modifyVariable x value (moveToStaticParentLevel))
        (stack1@stack2, callstack1@callstack2)
