<expr> ::=
| <expr> [ <nat> ]

MatchExpr : END
| PIPE CondExpr THINARR Expr MatchExpr (CondExpr(CondExpr) Expr(Expr) MatchExpr(MatchExpr))

nonterms
     | Args of plcType * string 


logo apos a ultima atomExpr:

    | FN Args FATARR Expr END (Anon(Args, Expr)) */

 Args : LPAREN RPAREN (List([]))
    | LPAREN Params RPAREN (Params)
    