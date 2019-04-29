# 1 "a2.mll"
 exception InvalidToken
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

# 16 "a2.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\242\255\243\255\249\255\251\255\000\000\253\255\009\000\
    \023\000\077\000\254\255\087\000\002\000\029\000\029\000\033\000\
    \038\000\250\255\044\000\040\000\035\000\087\000\035\000\040\000\
    \080\000\246\255\050\000\248\255\042\000\052\000\247\255\063\000\
    \056\000\054\000\066\000\066\000\057\000\065\000\087\000\054\000\
    \072\000\057\000\057\000\245\255\063\000\076\000\244\255\252\255\
    ";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\255\255\013\000\255\255\002\000\
    \002\000\001\000\255\255\013\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_default =
   "\001\000\000\000\000\000\000\000\000\000\255\255\000\000\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\000\000\000\000\
    ";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\012\000\012\000\012\000\012\000\012\000\000\000\012\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \012\000\000\000\012\000\000\000\000\000\000\000\000\000\000\000\
    \004\000\003\000\000\000\011\000\000\000\011\000\000\000\000\000\
    \010\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\005\000\000\000\000\000\047\000\000\000\000\000\
    \000\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \007\000\006\000\008\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\018\000\013\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\010\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\014\000\015\000\016\000\017\000\019\000\020\000\021\000\
    \022\000\031\000\023\000\028\000\025\000\027\000\029\000\030\000\
    \032\000\033\000\034\000\035\000\036\000\037\000\038\000\039\000\
    \044\000\041\000\024\000\042\000\043\000\040\000\045\000\046\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\026\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    ";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\012\000\012\000\000\000\255\255\012\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\012\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\255\255\000\000\255\255\000\000\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\005\000\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\007\000\008\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\013\000\014\000\015\000\016\000\018\000\019\000\020\000\
    \021\000\022\000\021\000\023\000\024\000\026\000\028\000\029\000\
    \031\000\032\000\033\000\034\000\035\000\036\000\037\000\038\000\
    \039\000\040\000\021\000\041\000\042\000\038\000\044\000\045\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\024\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec read lexbuf =
   __ocaml_lex_read_rec lexbuf 0
and __ocaml_lex_read_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 30 "a2.mll"
             (read lexbuf)
# 153 "a2.ml"

  | 1 ->
let
# 31 "a2.mll"
              i
# 159 "a2.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 31 "a2.mll"
                ( if(String.get i 0 = '+') then  INT( int_of_string (String.sub i 1 (String.length(i)-1)))::(read lexbuf) else   INT(int_of_string i):: (read lexbuf))
# 163 "a2.ml"

  | 2 ->
let
# 32 "a2.mll"
                id
# 169 "a2.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 32 "a2.mll"
                   ( ID(id):: (read lexbuf))
# 173 "a2.ml"

  | 3 ->
# 33 "a2.mll"
         (ASSIGN::(read lexbuf))
# 178 "a2.ml"

  | 4 ->
# 34 "a2.mll"
     (LP::(read lexbuf))
# 183 "a2.ml"

  | 5 ->
# 35 "a2.mll"
         (RETURN::(read lexbuf))
# 188 "a2.ml"

  | 6 ->
# 36 "a2.mll"
     (RP::(read lexbuf))
# 193 "a2.ml"

  | 7 ->
# 37 "a2.mll"
        (Stk::(read lexbuf))
# 198 "a2.ml"

  | 8 ->
# 38 "a2.mll"
            (Cstk::(read lexbuf))
# 203 "a2.ml"

  | 9 ->
# 39 "a2.mll"
         (PSlinks::(read lexbuf))
# 208 "a2.ml"

  | 10 ->
# 40 "a2.mll"
         (GVar::(read lexbuf))
# 213 "a2.ml"

  | 11 ->
# 41 "a2.mll"
          (GProc::(read lexbuf))
# 218 "a2.ml"

  | 12 ->
# 42 "a2.mll"
      ([])
# 223 "a2.ml"

  | 13 ->
# 43 "a2.mll"
    (raise InvalidToken)
# 228 "a2.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rec lexbuf __ocaml_lex_state

;;

# 44 "a2.mll"
 
  let scanner s = read (Lexing.from_string s);;

# 239 "a2.ml"
