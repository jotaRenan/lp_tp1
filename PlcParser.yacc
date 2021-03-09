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
    | TRUE | FALSE
    | EOF
    

%nonterm Prog of expr | Expr of expr | AtomExpr of expr | Const of expr | Decl of expr

%right SEMIC THINARR CSEQ
%left PLUS MINUS MULTI DIV LT LTE EQ NEQ AND ELSE LSB

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
    | Decl (Decl)


Decl : VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))

Expr : AtomExpr (AtomExpr)
    | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    | Expr EQ Expr (Prim2("=", Expr1, Expr2))
    | Expr MULTI Expr (Prim2("*", Expr1, Expr2))
    | Expr DIV Expr (Prim2("/", Expr1, Expr2))
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | Expr AND Expr (Prim2("&&", Expr1, Expr2))
    | Expr NEQ Expr (Prim2("!=", Expr1, Expr2))
    | Expr LT Expr (Prim2("<", Expr1, Expr2))
    | Expr LTE Expr (Prim2("<=", Expr1, Expr2))
    | MINUS Expr (Prim1("-", Expr1))
    | PRINT Expr (Prim1("print", Expr))
    | FALSE (ConB(false))
    | TRUE (ConB(true))


AtomExpr : Const (Const)
    | NAME (Var(NAME))
    | LPAREN Expr RPAREN (Expr)

Const : CINT (ConI(CINT))

