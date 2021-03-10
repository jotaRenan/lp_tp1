<expr> ::=
| <expr> [ <nat> ]

MatchExpr : END
| PIPE CondExpr THINARR Expr MatchExpr (CondExpr(CondExpr) Expr(Expr) MatchExpr(MatchExpr))

Args : LPAREN RPAREN
| LPAREN Params RPAREN (Params(Params))
