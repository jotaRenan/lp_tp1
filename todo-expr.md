<expr> ::=
| match <expr> with <matchexpr> match expression
| <expr> [ <nat> ]

Types : Type COMMA Type (ListT(Type1::Type2::[]))
| Type COMMA Types (ListT(Type::Types))

Type : AtomType (AtomType)
| LPAREN Types RPAREN (ListT(Types))
| LSB Type RSB (SeqT(Type))
| Type THINARR Type (FunT(Type1, Type2))

AtomType : NIL (ListT([]))
| BOOL (BoolT)
| INT (IntT)
| LPAREN Type RPAREN (Type)
