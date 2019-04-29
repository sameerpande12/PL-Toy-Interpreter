{exception InvalidToken
  type token = INT of int
| ASSIGN
| ID of string
}
let digits = ['0'-'9']
let nonzeroDigits = ['1'-'9']
let integers = ['+' '-']? ('0'|(nonzeroDigits digits*))

let whitespace = [' ' '\t' '\n' '\r']+
let assign = ":="
let identifier = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9']*
rule read = parse
  whitespace {read lexbuf}
| integers as i { if(String.get i 0 = '+') then  INT( int_of_string (String.sub i 1 (String.length(i)-1)))::(read lexbuf) else   INT(int_of_string i):: (read lexbuf)}
| identifier as id { ID(id):: (read lexbuf)}
| assign {ASSIGN::(read lexbuf)}
| _ {raise InvalidToken}
| eof {[]}
{
  let scanner s = read (Lexing.from_string s);;
}
