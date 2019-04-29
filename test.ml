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

let stack = [N(0);N(0);N(0)]@[REG([0;0;0;0])]@[getID main]@[N(0);N(0)] ;;
let callstack = [main] ;;
let fp = 3 ;;

let (stack,callstack,fp) = callProcedure "P" [N(0);N(0)] stack callstack fp;;

let printElement x = match x with
    N(y) -> Printf.printf "    %d\n" y

  | ID(name,parameters,locals) -> Printf.printf "fp->%s\n" name
  | REG(mlist) ->   Printf.printf "Reg-(0,0,0,0)\n";;


let printCaller x = match x with
    Node(y,_,_,_) -> Printf.printf "%s\n" y
  | Empty -> Printf.printf "Empty\n";;

let funcName x = match x with
    Node (y,_,_,_) -> y
  | Empty -> "EMPTY";;

let printStack stk = List.iter printElement stk;;
let printCallerStack cstk = List.iter printCaller cstk;;

let rec printSL stack callstack fp =
  if  callstack = [] then
    (Printf.printf "\n";)
  else
    (  Printf.printf "fp = %d -> %s\n" fp (funcName (List.hd callstack));

       let (stack1,callstack1,fp1) = moveToStaticParentLevel stack callstack fp in

       printSL stack1 callstack1 fp1

    )
;;
printSL stack callstack fp;;
getAllAccessibleVariables (stack,callstack,fp);;
