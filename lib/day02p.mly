%token <int> INT
%token <int> COLOR
%token COMMA
%token COLON
%token SEMICOLON
%token NEWLINE
%token GAME
%token EOF
%start <(int * int) list list list> prog
%%

prog:
    g = separated_nonempty_list(NEWLINE, game); EOF { g }

game:
    GAME; INT; COLON; r = separated_nonempty_list(SEMICOLON, round) { r }

round:
    r = separated_nonempty_list(COMMA, item) { r }

item:
    n = INT; c = COLOR { (c, n) }
