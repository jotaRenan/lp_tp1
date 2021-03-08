%%

%name PlcParser

%pos int

%term SEMIC
    | VAR | FUN | FUNREC
    | COL | IF | THEN | ELSE
    | MATCH | WITH
    | EXC
    | HEAD | TAIL | ISE
    | PRINT
    | PLUS | MINUS | MULTI | DIV | AND
    | EQ | NEQ | LT | LTE 
    | CSEQ
    | LSB | RSB | LB | RB | LPAREN | RPAREN
    | FN | FATARR | THINARR | END
    | COMMA | PIPE | UNDER
    | NAME of string | CINT of int
    | EOF
    

%nonterm Prog of expr | Expr of expr | AtomExpr of plcVal | App of expr
    | Decl of expr | Const of expr

%right SEMIC
%left PLUS MINUS

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
    | VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))

Expr : AtomExpr (AtomExpr)
    | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    | Expr EQ Expr (Prim2("=", Expr1, Expr2))

AtomExpr : Const (Const)
    | NAME (Var(NAME))
    | LB Prog RB Prog(Prog)
    | LPAREN Expr RPAREN Expr(Expr)

Const : CINT (IntV(CINT))
