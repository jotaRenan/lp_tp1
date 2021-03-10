%%

%name PlcParser

%pos int

%term SEMIC
    | VAR | FUN | FUNREC
    | COL | IF | THEN | ELSE 
    | MATCH | WITH
    | EXC | BOOL | INT | NIL
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
    | TypedVar of plcType * string | AppExpr of expr | Type of plcType | Types of plcType list 
    | AtomType of plcType | Params of (plcType * string) list | Comps of expr list

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
    | AppExpr (AppExpr)
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
    | MINUS Expr (Prim1("-", Expr))
    | PRINT Expr (Prim1("print", Expr))
    | FALSE (ConB(false))
    | TRUE (ConB(true))
    | EXC Expr (Prim1("!", Expr))
    | HEAD Expr (Prim1("hd", Expr))
    | TAIL Expr (Prim1("tl", Expr))
    | ISE Expr (Prim1("ise", Expr))
    | Expr CSEQ Expr (Prim2("::", Expr1, Expr2))
    | Expr SEMIC Expr (Prim2(";", Expr1, Expr2))
    | Expr LSB CINT RSB (Item(CINT, Expr))

AtomExpr : Const (Const)
    | NAME (Var(NAME))
    | LPAREN Expr RPAREN (Expr)
    | LPAREN Comps RPAREN (List(Comps))

AppExpr : AtomExpr AtomExpr (Call(AtomExpr1, AtomExpr2))
    | AppExpr AtomExpr (Call(AppExpr, AtomExpr))

Const : CINT (ConI(CINT))
    | FALSE (ConB(false))
    | TRUE (ConB(true))
    | LPAREN RPAREN (List([]))
    | LPAREN LSB Type RSB LSB RSB RPAREN (ESeq(SeqT(Type)))

Comps : Expr COMMA Expr (Expr1::Expr2::[])
    | Expr COMMA Comps (Expr::Comps)

Type : AtomType (AtomType)
    | LPAREN Types RPAREN (ListT(Types))
    | LSB Type RSB (SeqT(Type))
    | Type THINARR Type (FunT(Type1, Type2))

AtomType : NIL (ListT([]))
    | BOOL (BoolT)
    | INT (IntT)
    | LPAREN Type RPAREN (Type)

Types : Type COMMA Type (Type1::Type2::[])
    | Type COMMA Types (Type::Types)

TypedVar : Type NAME (Type, NAME)

Params : TypedVar (TypedVar::[])
    | TypedVar COMMA Params (TypedVar::Params)
