{exception InvalidToken
  type token = INT of int
| ASSIGN
| LP
| RP
| ID of char
| Stk (*print stack*)
| Cstk (*print callstack*)
| PSlinks (*print static links*)
| GVar (*get variables*)
| GProc (*get procedures *)
| RETURN
}
let digits = ['0'-'9']
let nonzeroDigits = ['1'-'9']
let integers = ['+' '-']? ('0'|(nonzeroDigits digits*))
let lp = '('
let rp = ')'
let whitespace = [' ' '\t' '\n' '\r']+
let assign = ":="
let identifier =  ['a'-'z' 'A'-'Z']
let return = "return"
let stack = "printStk"
let callstack = "printCstk"
let slinks = "printSL"
let getVar = "printAvailableVars"
let getProc = "printAvailableProc"

rule read = parse
  whitespace {read lexbuf}
| integers as i { if(String.get i 0 = '+') then  INT( int_of_string (String.sub i 1 (String.length(i)-1)))::(read lexbuf) else   INT(int_of_string i):: (read lexbuf)}
| identifier as id { ID(id):: (read lexbuf)}
| assign {ASSIGN::(read lexbuf)}
| lp {LP::(read lexbuf)}
| return {RETURN::(read lexbuf)}
| rp {RP::(read lexbuf)}
| stack {Stk::(read lexbuf)}
| callstack {Cstk::(read lexbuf)}
| slinks {PSlinks::(read lexbuf)}
| getVar {GVar::(read lexbuf)}
| getProc {GProc::(read lexbuf)}
| eof {[]}
| _ {raise InvalidToken}
{
  let scanner s = read (Lexing.from_string s);;
}
