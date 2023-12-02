{
    open Day02p
    exception SyntaxError of string
}

rule read = parse
| ' ' { read lexbuf }
| '\n' { NEWLINE }
| ':' { COLON }
| ';' { SEMICOLON }
| ',' { COMMA }
| "red" { COLOR(0) }
| "green" { COLOR(1) }
| "blue" { COLOR(2) }
| "Game "  { GAME }
| ['0'-'9']+ as i { INT(int_of_string i) }
| eof { EOF }
