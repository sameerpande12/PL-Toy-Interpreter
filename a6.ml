(* (key  ,leftchild ,rightchild, parent)  or empty node*)
exception InvalidParameterNumbers
exception InvalidProcedureName
exception EmptyStackException
exception InvalidStackException
exception InvalidFunctionCall
exception IncompatibleAssociation
exception EmptyNodeException
exception VariableNotFoundException
exception ElementNotFoundException
type funcNode = Node of (string * (string list) * (string list) * funcNode) | Empty
type stackElementType = N of int  | ID of (string * (string list) *(string list)) | REG of (int list)
(*funciton name , parameters list, local variables list*)





let getVariablesUsed fnode = match fnode with
    (_,parameters,locals,_)-> parameters @ locals



let rec getID x =

  match x with
    Node("main",[],["a";"b";"c"],Empty) ->ID("main",[],["a";"b";"c"])
  | Node("P",["x";"y"],["z";"a"],Node("main",[],["a";"b";"c"],Empty)) -> ID("P",["x";"y"],["z";"a"])
  | Node("Q",["z";"w"],["x";"b"],Node("main",[],["a";"b";"c"],Empty)) -> ID("Q",["z";"w"],["x";"b"])
  | Node("R",["w";"i"],["j";"b"],Node("P",["x";"y"],["z";"a"],Node("main",[],["a";"b";"c"],Empty)) ) -> ID("R",["w";"i"],["j";"b"])
  | Node("S",["c";"k"],["m";"n"],Node("P",["x";"y"],["z";"a"],Node("main",[],["a";"b";"c"],Empty)) ) -> ID("S",["c";"k"],["m";"n"])
  | Node("T",["a";"y"],["i";"f"],Node("Q",["z";"w"],["x";"b"],Node("main",[],["a";"b";"c"],Empty)) )  -> ID("T",["a";"y"],["i";"f"])
  | Node("U",["c";"z"],["p";"g"],Node("Q",["z";"w"],["x";"b"],Node("main",[],["a";"b";"c"],Empty)) ) -> ID("U",["c";"z"],["p";"g"])
  | Node("V",["m";"n"],["c"],Node("R",["w";"i"],["j";"b"],Node("P",["x";"y"],["z";"a"],Node("main",[],["a";"b";"c"],Empty)) ) ) -> ID("V",["m";"n"],["c"])
  | Node("W",["m";"p"],["j";"h"],Node("T",["a";"y"],["i";"f"],Node("Q",["z";"w"],["x";"b"],Node("main",[],["a";"b";"c"],Empty)) ) ) -> ID("W",["m";"p"],["j";"h"])
  | _ -> raise InvalidProcedureName






let getProcedure x = match x with
    "main" ->Node("main",[],["a";"b";"c"],Empty)
  | "P"->Node("P",["x";"y"],["z";"a"],Node("main",[],["a";"b";"c"],Empty))
  | "Q"->Node("Q",["z";"w"],["x";"b"],Node("main",[],["a";"b";"c"],Empty))
  | "R"->Node("R",["w";"i"],["j";"b"],Node("P",["x";"y"],["z";"a"],Node("main",[],["a";"b";"c"],Empty)) )
  | "S"->Node("S",["c";"k"],["m";"n"],Node("P",["x";"y"],["z";"a"],Node("main",[],["a";"b";"c"],Empty)) )
  | "T"->Node("T",["a";"y"],["i";"f"],Node("Q",["z";"w"],["x";"b"],Node("main",[],["a";"b";"c"],Empty)) )
  | "U"->Node("U",["c";"z"],["p";"g"],Node("Q",["z";"w"],["x";"b"],Node("main",[],["a";"b";"c"],Empty)) )
  | "V"->Node("V",["m";"n"],["c"],Node("R",["w";"i"],["j";"b"],Node("P",["x";"y"],["z";"a"],Node("main",[],["a";"b";"c"],Empty)) ) )
  | "W"->Node("W",["m";"p"],["j";"h"],Node("T",["a";"y"],["i";"f"],Node("Q",["z";"w"],["x";"b"],Node("main",[],["a";"b";"c"],Empty)) ) )
  | _ -> raise InvalidProcedureName



let rec getAllAncestors fnode =
  match fnode with
    Empty -> []
  | Node(_,_,_,pnode) -> fnode::(getAllAncestors pnode)


let getParent fnode = match fnode with
    Node(_,_,_,x)->x
  | Empty -> raise EmptyNodeException


let rec isMember a mlist = match mlist with
    [] -> false
  | m::mlist1 -> if(a=m)then true else isMember a mlist1

let rec union l1 l2 = match l1 with
    [] -> l2
  | l::list1 -> if(isMember l l2)then (union list1 l2 )
    else union list1 (l::l2)


let canCall pnode qnode = (*asks if pnode can call qnode*)

  let ancestors = getAllAncestors pnode in
  let parent = getParent qnode in
  (isMember qnode ancestors) || (isMember parent ancestors)


let rec generateZeros n =
  if(n = 0)then []
  else N(0)::(generateZeros (n-1))

let getLocalVariablesSize x = match x with
    Node(_,_,l,_)-> List.length l
  | Empty ->0
let getLocals x = match x with
    Node(_,_,l,_) -> l
  | Empty -> []
let getParameters x = match x with
    Node(_,para,_,_) -> para
  | Empty -> []


let getParametersSize x = match x with
    Node(_,para,_,_)-> List.length para
  | Empty ->0

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
  |n::nlist1 -> if (getParent p) = n then nlist else removeNodesBeforeParent p nlist1

let getFrameSize fnode = match fnode with
    Node(name,parameters,locals,_)-> 4 + (List.length parameters)+ (List.length locals)
  | Empty -> 0

let rec addList mlist = match mlist with
    []->0
  |m::mlist1 -> m + addList mlist1
let callProcedure pname parameters stack callstack fp =  (*returns a tuple of modified stack , call stack, fp*)
  let callee = getProcedure pname in
  let caller = List.hd callstack in
  if canCall caller (callee) then
    let toMoveFromFpDL =  2 + (List.length parameters) + (getLocalVariablesSize (List.hd callstack)) + 1 + 1 in

    let toMoveFromFpSL =
          let nodesTillParent = getNodesTillParent callee callstack in
          (2 + getParametersSize callee)  +  addList(List.map getFrameSize nodesTillParent) - (2 + getParametersSize (List.hd (List.rev nodesTillParent)))
    in
    ( (generateZeros (getLocalVariablesSize callee))@[REG([0;0;0;0])]@[getID callee]@[N (toMoveFromFpDL)]@[N(toMoveFromFpSL)]@parameters@stack , callee::callstack, fp + toMoveFromFpDL)
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
  | ((s,t)::l1) -> if (s = x)  then true else isKeyIn x l1

let rec union l1 l2 = match l1 with (*union of l2 and l1 keys preferring value of l2*)
    [] -> l2
  | ((k1,v1)::list1) ->
    if isKeyIn k1 l2 then
      union list1 l2
    else (List.hd l1)::(union list1 l2)

let  currentVariableValuePairs stack callstack =
  let topFunction = List.hd callstack in
  let localAndParameters = match topFunction with
      Node(_,parameters,locals,parent) ->(locals,parameters)
  | _ ->([],[])in
  let parameterValues = getFirstN (getParametersSize topFunction) (removeFirstN 3 (removeBeforeId stack)) in
  let parameterVariables = snd localAndParameters in

  let elementsTillId = numElementsTillId stack in
  let localValues = removeFirstN (elementsTillId -1 - (getLocalVariablesSize topFunction)) (getFirstN (elementsTillId - 1 ) stack) in
  let localVariables = fst localAndParameters in

  let localvariableValuePairs = createAssociation localVariables localValues in
  let parametervariableValuePairs = createAssociation parameterVariables parameterValues in

   union parametervariableValuePairs localvariableValuePairs

let moveToStaticParentLevel stack callstack fp =
  if(List.tl callstack = [])then ([],[],0)
  else(
    let toMoveFromFpSL = match   List.hd(List.tl (List.tl (removeBeforeId stack))) with N(x)-> x | _ -> raise InvalidStackException in
    let modifiedStack = (removeBeforeId stack) in
    let newCallStack = removeNodesBeforeParent (List.hd callstack) callstack in (*keeps the parent*)
    let modifiedStack = (removeFirstN (toMoveFromFpSL -1 - getLocalVariablesSize(List.hd newCallStack)) modifiedStack) in
    (modifiedStack,newCallStack,fp - toMoveFromFpSL)
  )

let getStacksBeforeStaticParent  stack callstack =
  if(List.tl callstack = []) then (stack,callstack)
  else
   let toMoveFromFpSL = match   List.hd(List.tl (List.tl (removeBeforeId stack))) with N(x)-> x | _ -> raise InvalidStackException in

   let modifiedStack =   (removeBeforeId stack) in
   let newCallStack = getNodesBeforeParent (List.hd callstack) callstack in (*keeps the parent*)
   let modifiedStack = (getBeforeId stack)@(getFirstN (toMoveFromFpSL -1 - getLocalVariablesSize(List.hd newCallStack)) modifiedStack) in

   (modifiedStack,newCallStack)



let rec getAllAccessibleVariables (stack, callstack, fp) =
  if(List.tl callstack = [])then (currentVariableValuePairs stack callstack)
  else
    union (getAllAccessibleVariables(moveToStaticParentLevel stack callstack fp))  (currentVariableValuePairs stack callstack)

let returnBack stack callstack fp=  (*gives current frame pointer in fp*)
  let topProcedure = List.hd callstack in
  let modifiedStack = removeFirstN (3 +  (getParametersSize topProcedure)) (removeBeforeId stack) in
  let toMoveFromFpDL = match List.hd(List.tl (removeBeforeId stack)) with
      N(x)-> x
    |_ -> raise InvalidStackException  in
  (modifiedStack,  List.tl callstack  , fp - toMoveFromFpDL)


let rec getPosition x mlist = match mlist with
    [] -> raise ElementNotFoundException
  | m::n -> if (m = x )then 1 else (1 + getPosition x n)

let rec modifyVariable x value (stack,callstack,fp) =

  let localVbls = getLocals (List.hd callstack) in
  let parameterVbls = getParameters (List.hd callstack) in
  if isMember x localVbls then
    let rankInLocals = getPosition x  localVbls in
    let elementsBeforeVariable = numElementsTillId stack -1 -getLocalVariablesSize (List.hd callstack) + rankInLocals - 1 in
    ((getFirstN elementsBeforeVariable stack )@[N(value)]@ (removeFirstN (elementsBeforeVariable+1)  stack) , callstack)
  else if(isMember x parameterVbls) then
    let rankInPara = getPosition x parameterVbls in
    let elementsBeforeVariable = numElementsTillId stack + 1 + 2 + rankInPara-1 in

    ((getFirstN elementsBeforeVariable stack )@[N(value)]@ (removeFirstN (elementsBeforeVariable+1)  stack) , callstack)
  else
    if(List.tl callstack = [])then raise VariableNotFoundException
    else
        let (stack1,callstack1) =  getStacksBeforeStaticParent stack callstack in
        let (stack2,callstack2) =  modifyVariable x value  ( moveToStaticParentLevel stack callstack fp ) in

        (stack1@stack2,callstack1@callstack2)
