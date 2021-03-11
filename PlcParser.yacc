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
    | MatchExpr of (expr option * expr) list | CondExpr of (expr option) 
    | Args of (plcType * string) list

%right SEMIC THINARR 
%nonassoc IF
%left ELSE
%left AND
%left EQ NEQ
%left LT LTE
%right CSEQ
%left PLUS MINUS
%left MULTI DIV 
%nonassoc EXC HEAD TAIL ISE PRINT NAME
%left LSB

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
    | Decl (Decl)


Decl : VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))
    | FUN NAME Args EQ Expr SEMIC Prog (Let(NAME, makeAnon(Args, Expr), Prog))
    | FUN FUNREC NAME Args COL Type EQ Expr SEMIC Prog (makeFun(NAME, Args, Type, Expr, Prog))
    

Expr : AtomExpr (AtomExpr)
    | AppExpr (AppExpr)
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
    | EXC Expr (Prim1("!", Expr))
    | MINUS Expr (Prim1("-", Expr))
    | HEAD Expr (Prim1("hd", Expr))
    | TAIL Expr (Prim1("tl", Expr))
    | ISE Expr (Prim1("ise", Expr))
    | PRINT Expr (Prim1("print", Expr))
    | Expr AND Expr (Prim2("&&", Expr1, Expr2))
    | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    | Expr MULTI Expr (Prim2("*", Expr1, Expr2))
    | Expr DIV Expr (Prim2("/", Expr1, Expr2))
    | Expr EQ Expr (Prim2("=", Expr1, Expr2))
    | Expr NEQ Expr (Prim2("!=", Expr1, Expr2))
    | Expr CSEQ Expr (Prim2("::", Expr1, Expr2))
    | Expr LT Expr (Prim2("<", Expr1, Expr2))
    | Expr LTE Expr (Prim2("<=", Expr1, Expr2))
    | Expr SEMIC Expr (Prim2(";", Expr1, Expr2))
    | Expr LSB CINT RSB (Item(CINT, Expr))

AtomExpr : Const (Const)
    | NAME (Var(NAME))
    | LB Prog RB (Prog)
    | LPAREN Expr RPAREN (Expr)
    | LPAREN Comps RPAREN (List(Comps))
    | FN Args FATARR Expr END (makeAnon(Args, Expr))

MatchExpr : END ([])
    | PIPE CondExpr THINARR Expr MatchExpr ((CondExpr, Expr)::MatchExpr)

CondExpr : Expr (SOME Expr)
    | UNDER (NONE)

AppExpr : AtomExpr AtomExpr (Call(AtomExpr1, AtomExpr2))
    | AppExpr AtomExpr (Call(AppExpr, AtomExpr))

Const : FALSE (ConB(false))
    | TRUE (ConB(true))
    | CINT (ConI(CINT))
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

Args : LPAREN RPAREN ([])
    | LPAREN Params RPAREN (Params)