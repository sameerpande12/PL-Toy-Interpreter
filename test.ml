(*
Frame structure

      Local Variables  (arragned top to bottom in the same order they appear in ID in Left to right order. example if [a,b] then a
                                                                                                                                 b        )
      Registers(0,0,0,0) (for use generally, though not used in this assignment)
fp -> ID(name,parameters,locals)
       integerValue by how much we should move down from fp to go to previous functions frame pointer
       integerValue by how much we should move down from fp to go to previous statically linked frame pointer
       Parameters (in the order as: if ID contains [x,y] then   x   in the stack
                                                                y           )

*)

#load "a6.cmo";;

open A6;;

let main = Node("main",[],["a";"b";"c"],Empty) ;;
let p =Node("P",["x";"y"],["z";"a"],main);;
let q =Node("Q",["z";"w"],["x";"b"],main);;
let r =Node("R",["w";"i"],["j";"b"],p);;
let s =Node("S",["c";"k"],["m";"n"],p);;
let t =Node("T",["a";"y"],["i";"f"],q);;
let u =Node("U",["c";"z"],["p";"g"],q);;
let v =Node("V",["m";"n"],["c"],r);;
let w =Node("W",["m";"p"],["j";"h"],t);;
let allProcedures = [main;p;q;r;s;t;u;v;w];;

let stack = [N(0);N(0);N(0)]@[REG([0;0;0;0])]@[getID main]@[N(0);N(0)] ;;
let callstack = [main] ;;
let fp = 3 ;;

let rec printVariables mlist = match mlist with
    []-> Printf.printf "\n"
  | (key,value)::mlist1-> Printf.printf "(%s,%d), " key value; printVariables mlist1

let rec printStringList mlist = match mlist with
    [] -> Printf.printf " "
  | x::mlist1 -> Printf.printf "%s " x ; printStringList mlist1

let printElement x = match x with
    N(y) -> Printf.printf "    %d\n" y

  | ID(name,parameters,locals) -> (Printf.printf "fp->%s " name;
    Printf.printf "parameters -> ";
    printStringList parameters;
    Printf.printf "locals -> ";
                                   printStringList locals;
                                   Printf.printf "\n"
                                  )

  | REG(mlist) ->   Printf.printf "Reg-(0,0,0,0)\n";;


let printCaller x = match x with
    Node(y,_,_,_) -> Printf.printf "%s\n" y
  | Empty -> Printf.printf "Empty\n";;

let funcName x = match x with
    Node (y,_,_,_) -> y
  | Empty -> "EMPTY";;

let printStack stk = List.iter printElement stk;;
let printCstk cstk = List.iter printCaller cstk;;

let rec printSL stack callstack fp =
  if  callstack = [] then
    (Printf.printf "\n";)
  else
    (  Printf.printf "fp = %d -> %s\n" fp (funcName (List.hd callstack));

       let (stack1,callstack1,fp1) = moveToStaticParentLevel stack callstack fp in

       printSL stack1 callstack1 fp1

    )
;;

let printAllAccessibleProcedures x =
  let tempPrint (procedure,boolval) = match procedure with
      Node(y,_,_,_) ->   if(boolval) then Printf.printf "%s -> Accessible\n" y
      else Printf.printf "%s -> Unaccessible\n" y
    | Empty -> Printf.printf "Empty procedure\n"
  in

  List.iter tempPrint (createAssociation allProcedures (List.map (canCall x) allProcedures))
;;


let (stack,callstack,fp) = callProcedure "P" [N(0);N(0)] stack callstack fp;;
printSL stack callstack fp;;
getAllAccessibleVariables (stack,callstack,fp);;
let (stack,callstack,fp) = returnBack stack callstack fp;;

let (stack,callstack,fp) = callProcedure "P" [N(1);N(2)] stack callstack fp;;
let (stack,callstack,fp) = callProcedure "Q" [N(3);N(4)] stack callstack fp;;
let (stack,callstack,fp) = callProcedure "T" [N(5);N(6)] stack callstack fp;;
let (stack,callstack,fp) = callProcedure "W" [N(7);N(8)] stack callstack fp;;
let (stack,callstack,fp) = callProcedure "P" [N(9);N(10)] stack callstack fp;;


printCstk (getNodesTillParent (List.hd callstack) callstack);;

let (stack,callstack) = modifyVariable "x" N(11) (stack,callstack,fp);;
printStack stack;;
let (stack,callstack) = modifyVariable "z" N(-2) (stack,callstack,fp);;
printStack stack;;
let (stack,callstack) = modifyVariable "b" N(-23) (stack,callstack,fp);;
printStack stack;;
let (stack,callstack,fp) = callProcedure "R" [N(11);N(12)] stack callstack fp;;
printStack stack;;

let (stack,callstack) = modifyVariable "x" N(1111) (stack,callstack,fp);;
let (stack,callstack) = modifyVariable "a" N(-999) (stack,callstack,fp);;



getAllAccessibleVariables(stack,callstack,fp);;(*notice the value of b is 0 and not -23(from main)*)
let (stack,callstack,fp) = returnBack stack callstack fp;;
getAllAccessibleVariables(stack,callstack,fp);;(*notice the value of b is now -23*)
