{
   open SymCamlParser
   open Printf
}

let decimal =  ['-']? ['0'-'9']+'.'['0'-'9']+
let integer = ['-']? ['0'-'9']+'.'?
let token = ['A'-'Z' 'a'-'z' '_']+['A'-'Z' 'a'-'z' '_' '0'-'9' ':' '|' '.']*
let str = ['''][^ ''']*[''']
let qstr = ['"'][^ '"']*['"']

rule main = parse
  |[' ''\t''\n'] {main lexbuf}
  | [','] {COMMA}
  | ['='] {EQ}
  | ['('] {OPARAN}
  | [')'] {CPARAN}
  | ['"'] {QUOTE}
  | "Function" {FUNCTION}
  | token as word {
     flush_all();
     TOKEN(word)
  }
  | str as word {
     let qword = List.nth (Str.split (Str.regexp "\'") word) 0 in
     flush_all();
     QTOKEN(qword)
  }
  | qstr as word {
    let qword = List.nth (Str.split (Str.regexp "\'") word) 0 in
    flush_all();
    QTOKEN(qword)
  }
  | decimal as dec {DECIMAL(float_of_string dec)}
  | integer as i   {INTEGER(int_of_string i)}
  | eof {EOF}
