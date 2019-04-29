#load "a6.cmo";;
#load "a2.cmo";;

open A6;;
open A2;;
let scan s = A2.read (Lexing.from_string s)in
let stack = [N(0);N(0);N(0)]@[REG([0;0;0;0])]@[getID main]@[N(0);N(0)] in
let callstack = [main] in
let fp = 3 in

while true do
  let s = read_line () in
  let tokens = scan s in
  match tokens with
    [ID(var);ASSIGN;ID(var)] ->

    [ID(var);ASSIGN;INT(x)] ->

    [ID(var);]


done
