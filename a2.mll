{exception InvalidToken
  type token = INT of int
| ASSIGN
| LP
| RP
| ID of string
| PrintStack (*print stack*)
| PrintCallStack (*print callstack*)
| PrintStaticlinks (*print static links*)
| GetAllAccessibleVbls (*get variables*)
| GetAllAccessibleProcs (*get procedures *)
| RETURN
| FP
| COMMA
}
let digits = ['0'-'9']
let nonzeroDigits = ['1'-'9']
let integers = ['+' '-']? ('0'|(nonzeroDigits digits*))
let lp = '('
let rp = ')'
let comma = ','
let whitespace = [' ' '\t' '\n' '\r']+
let assign = ":="
let identifier =  ['a'-'z' 'A'-'Z']+
let return = "return"
let stack = "stack()"
let callstack = "callStack()"
let slinks = "staticLinks()"
let getVar = "variables()"
let getProc = "procedures()"
let fp = "fp()"
rule read = parse
  whitespace {read lexbuf}
| integers as i { if(String.get i 0 = '+') then  INT( int_of_string (String.sub i 1 (String.length(i)-1)))::(read lexbuf) else   INT(int_of_string i):: (read lexbuf)}
| return {RETURN::(read lexbuf)}
| identifier as id { ID(id):: (read lexbuf)}
| assign {ASSIGN::(read lexbuf)}
| lp {LP::(read lexbuf)}
| rp {RP::(read lexbuf)}
| stack {PrintStack::(read lexbuf)}
| callstack {PrintCallStack::(read lexbuf)}
| slinks {PrintStaticlinks::(read lexbuf)}
| getVar {GetAllAccessibleVbls::(read lexbuf)}
| getProc {GetAllAccessibleProcs::(read lexbuf)}
| fp {FP::(read lexbuf)}
| comma {COMMA::(read lexbuf)}
| eof {[]}
| _ {raise InvalidToken}
{
  let scanner s = read (Lexing.from_string s);;
}
