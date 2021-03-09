<expr> ::=
| match <expr> with <matchexpr> match expression
| <expr> [ <nat> ]

Params : TypedVar (TypedVar(TypedVar))
| TypedVar COMMA Params (TypedVar(TypedVar) Params(Params))

TypedVar : Type NAME (Type(Type) Var(NAME))

MatchExpr : END
| PIPE CondExpr THINARR Expr MatchExpr (CondExpr(CondExpr) Expr(Expr) MatchExpr(MatchExpr))

Args : LPAREN RPAREN
| LPAREN Params RPAREN (Params(Params))
