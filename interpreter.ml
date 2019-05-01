#load "a6.cmo";;
#load "a2.cmo";;
open A6;;
open A2;;

Printf.printf
"Welcome!
Please follow the following rules to run this interpreter.
=================================================================================================
Commands     |        Function
=================================================================================================
stack()      |       Prints the current stack
callStack()  |       Prints the current callStack
return       |       Returns fromt the current procedure
variables()  |       Prints list of all accessible variables
             |       (from procedure on top of callStack)
             |        with values
procedures() |       Prints all procedures accessible from top
             |       of the callStack
fp()         |       Prints current frame pointer value
staticLinks()|       Prints the frame pointer value of all members of
             |       static link chain
x:= {integer}|       Assignment of an integer to a variable
             |       example a:=3
x:=y         |       Assignment of variable value to variable
             |       example a:=b
P(a,b)       |       Calling procedure with two parameters
             |       P is the procedure name
P()          |       Call procedure with no parameters
==================================================================================================
\n\n
Here are some details about how the frame looks in this Assignment

         Local Variable Values (*arranged top-down in the same order as their
                                names appear in ID (from left to right)  *)
         Reg(0,0,0,0)  (*local registers*)
    fp-> ID(name,parameter names, local variable names)
         toMoveFromFpDL  (integer representing number of pops required starting from \"fp\" to move one dynamic line down )
         toMoveFromFpSL  (integer representing number of pops required starting from \"fp\" to move one static line down )
         Parameter Variable Values (*arranged top-down in the same order as their
                                names appear in ID (from left to right)  *)


example:- when callStack is [main]

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+          0  (*value of a*)                            +
+          0  (*value of b*)                            +
+          0  (*value of c*)                            +
+          Reg(0,0,0)                                   +
+   fp=3 -ID(\"main\",[],[\"a\";\"b\";\"c\"])
+          0  (*set 0 since no movement through DL*)    +
+          0  (*set 0 since no movement through SL*)    +
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

upon calling P(1,2)

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+          0  (*value of z*)                                                       +
+          0  (*value of a*)                                                       +
+   fp=12- ID (\"P\",[\"x\";\"y\"],[\"z\";\"a\"])
+          9  (*moving 9 steps down from fp reaches fp of main via Dynamic Link*)  +
+          9  (*moving 9 steps down from fp reaches fp of main via Static Link*)   +
+          1  (*value of x*)                                                       +
+          2  (*value of y*)                                                       +
+          0  (*value of a*)                                                       +
+          0  (*value of b*)                                                       +
+          0  (*value of c*)                                                       +
+          Reg(0,0,0)                                                              +
+          ID(main,[],[\"a\";\"b\";\"c\"])
+          0  (*set 0 since no movement through DL*)                               +
+          0  (*set 0 since no movement through SL*)                               +
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


";

let main = Node("main",[],["a";"b";"c"],Empty) in
let p =Node("P",["x";"y"],["z";"a"],main)in
let q =Node("Q",["z";"w"],["x";"b"],main)in
let r =Node("R",["w";"i"],["j";"b"],p)in
let s =Node("S",["c";"k"],["m";"n"],p)in
let t =Node("T",["a";"y"],["i";"f"],q)in
let u =Node("U",["c";"z"],["p";"g"],q)in
let v =Node("V",["m";"n"],["c"],r)in
let w =Node("W",["m";"p"],["j";"h"],t)in
let allProcedures = [main;p;q;r;s;t;u;v;w]in


let scan s = A2.read (Lexing.from_string s)in
let stack = ref ([N(0);N(0);N(0)]@[REG([0;0;0;0])]@[getID main]@[N(0);N(0)]) in
let callstack = ref([main]) in
let fp = ref(3) in


let rec printVariables mlist = match mlist with
    []-> Printf.printf "\n"
  | (key,N(value))::mlist1-> Printf.printf "(%s,%d), " key value; printVariables mlist1
  | _ -> Printf.printf ""
in
let rec printStringList mlist = match mlist with
    [] -> Printf.printf " "
  | x::mlist1 -> (Printf.printf "%s " x ); printStringList mlist1
in
let printElement x = match x with
    N(y) -> Printf.printf "    %d\n" y

  | ID(name,parameters,locals) -> (Printf.printf "fp->%s " name;
                                   Printf.printf "parameters -> ";
                                   printStringList parameters;
                                   Printf.printf "locals -> ";
                                   printStringList locals;
                                   Printf.printf "\n"
                                  )

  | REG(mlist) ->   Printf.printf "Reg-(0,0,0,0)\n"
in

let printCaller x = match x with
    Node(y,_,_,_) -> Printf.printf "%s\n" y
  | Empty -> Printf.printf "Empty\n"
in
let funcName x = match x with
    Node (y,_,_,_) -> y
  | Empty -> "EMPTY"
in
let printStack stk = List.iter printElement stk in
let printCstk cstk = List.iter printCaller cstk in

let rec printSL stack callstack fp =
  if  callstack = [] then
    (Printf.printf "\n";)
  else
    (  Printf.printf "fp = %d -> %s\n" fp (funcName (List.hd callstack));

       let (stack1,callstack1,fp1) = moveToStaticParentLevel stack callstack fp in

       printSL stack1 callstack1 fp1

    )
in

let printAllAccessibleProcedures x =
  let tempPrint (procedure,boolval) = match procedure with
      Node(y,_,_,_) ->   if(boolval) then Printf.printf "%s -> Accessible\n" y
      else Printf.printf "%s -> Unaccessible\n" y
    | Empty -> Printf.printf "Empty procedure\n"
  in

  List.iter tempPrint (createAssociation allProcedures (List.map (canCall x) allProcedures))
in



let isInteger x  =
  match x with
    N(y)-> true
  | _ -> false
in
let assign3 (x,y,z) (a,b,c) =
  x:=a;
  y:=b;
  z:=c;
in


let assign2 (x,y) (a,b) =
  x:=a;
  y:=b;

in

let isProcedure var =
  if var = "main" || var = "P" ||var = "Q" ||var = "R" ||var = "S" ||var = "T" ||var = "U" ||var = "V" ||var = "W" then true
  else false
in
while true do
  Printf.printf ":-" ;
  let s = read_line () in
  let tokens = scan s in

  match tokens with
    [ID(var);ASSIGN;ID(y)] ->(
      if isProcedure var then
        (Printf.printf "%s is a Procedure and not a variable\n" var;)
      else if isProcedure y then
        (Printf.printf "%s is a Procedure and not a variable\n" y;)
      else
        let allAccessibleVbls = getAllAccessibleVariables (!stack,!callstack,!fp) in
        let isVarPresent = List.mem_assoc var allAccessibleVbls in
        let isyPresent = List.mem_assoc y allAccessibleVbls in

        match (isVarPresent,isyPresent) with
          (false,false)->(Printf.printf "%s and %s are not accessible\n" var y;)
        | (false,true)-> (Printf.printf "%s is not accessible\n" var;)
        | (true,false)-> (Printf.printf "%s is not accessible\n" y;)
        | (true,true)->
          let yVal = List.assoc y allAccessibleVbls in
          assign2 (stack,callstack) (modifyVariable var yVal (!stack,!callstack,!fp))
    )

  |[ID(var);ASSIGN;INT(x)] ->(
      if isProcedure var then
        (Printf.printf "%s is a Procedure and not a variable\n" var;)
      else
        let allAccessibleVbls = getAllAccessibleVariables (!stack,!callstack,!fp) in
        let isVarPresent = List.mem_assoc var allAccessibleVbls in
        if isVarPresent then
          assign2 (stack,callstack)  (modifyVariable var (N(x)) (!stack,!callstack,!fp))
        else
          (Printf.printf "%s is not accessible\n" var;)

    )


  |[ID(var);LP;ID(y);COMMA;ID(x);RP]->
    if isProcedure var then

      let allAccessibleVbls = getAllAccessibleVariables (!stack,!callstack,!fp) in
      let isVarPresent = List.mem_assoc y allAccessibleVbls in
      let isVarPresent = isVarPresent && (List.mem_assoc x allAccessibleVbls) in
      let procedure = getProcedure var in
      try
          if isVarPresent then
            (if (getParametersSize procedure) = 2 then
               let yVal = List.assoc y allAccessibleVbls in
               let xVal = List.assoc x allAccessibleVbls in
              (if isInteger yVal && isInteger xVal then
                 assign3 (stack,callstack,fp)  (callProcedure var [yVal;xVal] !stack !callstack !fp)
              else
                (Printf.printf "Please ensure the parameters are of type int\n"  )
              )
            else
              (Printf.printf "Incorrect numbers of parameters\n")
            )
          else
          (Printf.printf "Parameters not accessible\n" )
     with InvalidFunctionCall -> Printf.printf "%s cannot be accessed\n" var
    else
      (Printf.printf "%s is not a procedure\n" var)

  |  [ID(var);LP;ID(y);COMMA;INT(x);RP]->

    if isProcedure var then

      let allAccessibleVbls = getAllAccessibleVariables (!stack,!callstack,!fp) in
      let isVarPresent = List.mem_assoc y allAccessibleVbls in
      let procedure = getProcedure var in
      try
        if isVarPresent then
          (if (getParametersSize procedure) = 2 then
             let yVal = List.assoc y allAccessibleVbls in

             if isInteger yVal  then
               assign3 (stack,callstack,fp)  (callProcedure var [yVal;N(x)] !stack !callstack !fp)
              else
                (Printf.printf "Please ensure the parameters are of type int\n"  )

           else
             (Printf.printf "Incorrect numbers of parameters\n")
          )
        else
          (Printf.printf "Parameters not accessible\n" )
      with InvalidFunctionCall -> Printf.printf "%s cannot be accessed\n" var
    else
      (Printf.printf "%s is not a procedure\n" var)

  |  [ID(var);LP;INT(x);COMMA;ID(y);RP]->
    if isProcedure var then

      let allAccessibleVbls = getAllAccessibleVariables (!stack,!callstack,!fp) in
      let isVarPresent = List.mem_assoc y allAccessibleVbls in
      let procedure = getProcedure var in
      try
        if isVarPresent then
          (if (getParametersSize procedure) = 2 then
             let yVal = List.assoc y allAccessibleVbls in

             if isInteger yVal  then
               assign3 (stack,callstack,fp)  (callProcedure var [N(x);yVal] !stack !callstack !fp)
             else
               (Printf.printf "Please ensure the parameters are of type int\n"  )

           else
             (Printf.printf "Incorrect numbers of parameters\n")
          )
        else
          (Printf.printf "Parameters not accessible\n" )
      with InvalidFunctionCall -> Printf.printf "%s cannot be accessed\n" var
    else
      (Printf.printf "%s is not a procedure\n" var)


  |  [ID(var);LP;INT(x);COMMA;INT(y);RP]->
    if isProcedure var then
      try
        assign3 (stack,callstack,fp)  (callProcedure var [N(x);N(y)] !stack !callstack !fp)
      with InvalidFunctionCall -> Printf.printf "%s cannot be accessed\n" var

    else
      (Printf.printf "%s is not a procedure\n" var)
  | [ID(var);LP;RP]->(*to call main*)
    if isProcedure var then
      try
        assign3 (stack,callstack,fp)  (callProcedure var [] !stack !callstack !fp)
      with InvalidFunctionCall -> Printf.printf "%s cannot be accessed\n" var

    else
      (Printf.printf "%s is not a procedure\n" var)
  |  [PrintStack] ->Printf.printf "\n"; printStack !stack;
  |  [PrintCallStack] ->Printf.printf "\n"; printCstk !callstack;
  |  [PrintStaticlinks]->Printf.printf "\n"; printSL !stack !callstack !fp;
  |  [GetAllAccessibleVbls]->Printf.printf "\n"; printVariables (getAllAccessibleVariables (!stack,!callstack,!fp));
  |  [GetAllAccessibleProcs]->Printf.printf "\n";printAllAccessibleProcedures (List.hd !callstack);

  |  [RETURN]-> if (List.length !callstack < 2) then Printf.printf"Cannot Return. Call Stack will become empty\n" else assign3 (stack,callstack,fp)  (returnBack !stack !callstack !fp)
  |  [FP] -> Printf.printf "\nfp -> %d\n" !fp;
  | _ -> Printf.printf "Invalid Token\n"



done
