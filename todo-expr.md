<expr> ::=
| <expr> [ <nat> ]

MatchExpr : END
| PIPE CondExpr THINARR Expr MatchExpr (CondExpr(CondExpr) Expr(Expr) MatchExpr(MatchExpr))

logo apos a ultima atomExpr:

    | FN Args FATARR Expr END (Anon(Args, Expr)) */


(
    "f",
    IntT,
    "n",
    IntT,
    Prim2(
        "+",
        If (
            Prim2 ("<=",Var "n",ConI 0),
            ConI 0,
            Var "n"
        ),
        Call (Var "f",Prim2 ("-",Var "n",ConI 1))
    ),
    Call (Var "f",ConI 5)
)