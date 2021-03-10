functor PlcParserLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : PlcParser_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\116\000\007\000\116\000\008\000\116\000\019\000\116\000\
\\020\000\116\000\021\000\116\000\022\000\116\000\023\000\116\000\
\\024\000\116\000\025\000\116\000\026\000\116\000\027\000\116\000\
\\028\000\116\000\029\000\116\000\033\000\132\000\034\000\116\000\
\\039\000\116\000\042\000\132\000\043\000\132\000\044\000\132\000\
\\045\000\132\000\046\000\116\000\000\000\
\\001\000\001\000\117\000\007\000\117\000\008\000\117\000\019\000\117\000\
\\020\000\117\000\021\000\117\000\022\000\117\000\023\000\117\000\
\\024\000\117\000\025\000\117\000\026\000\117\000\027\000\117\000\
\\028\000\117\000\029\000\117\000\033\000\133\000\034\000\117\000\
\\039\000\117\000\042\000\133\000\043\000\133\000\044\000\133\000\
\\045\000\133\000\046\000\117\000\000\000\
\\001\000\001\000\036\000\007\000\071\000\019\000\035\000\020\000\034\000\
\\021\000\033\000\022\000\032\000\023\000\031\000\024\000\030\000\
\\025\000\029\000\026\000\028\000\027\000\027\000\028\000\026\000\
\\029\000\025\000\000\000\
\\001\000\001\000\036\000\008\000\089\000\019\000\035\000\020\000\034\000\
\\021\000\033\000\022\000\032\000\023\000\031\000\024\000\030\000\
\\025\000\029\000\026\000\028\000\027\000\027\000\028\000\026\000\
\\029\000\025\000\000\000\
\\001\000\001\000\036\000\019\000\035\000\020\000\034\000\021\000\033\000\
\\022\000\032\000\023\000\031\000\024\000\030\000\025\000\029\000\
\\026\000\028\000\027\000\027\000\028\000\026\000\029\000\025\000\
\\034\000\063\000\039\000\062\000\000\000\
\\001\000\001\000\036\000\019\000\035\000\020\000\034\000\021\000\033\000\
\\022\000\032\000\023\000\031\000\024\000\030\000\025\000\029\000\
\\026\000\028\000\027\000\027\000\028\000\026\000\029\000\025\000\
\\046\000\099\000\000\000\
\\001\000\001\000\090\000\019\000\035\000\020\000\034\000\021\000\033\000\
\\022\000\032\000\023\000\031\000\024\000\030\000\025\000\029\000\
\\026\000\028\000\027\000\027\000\028\000\026\000\029\000\025\000\000\000\
\\001\000\002\000\020\000\006\000\019\000\011\000\018\000\015\000\017\000\
\\016\000\016\000\017\000\015\000\018\000\014\000\020\000\013\000\
\\033\000\012\000\042\000\011\000\043\000\010\000\044\000\009\000\
\\045\000\008\000\000\000\
\\001\000\006\000\019\000\011\000\018\000\015\000\017\000\016\000\016\000\
\\017\000\015\000\018\000\014\000\020\000\013\000\029\000\040\000\
\\033\000\012\000\034\000\039\000\042\000\011\000\043\000\010\000\
\\044\000\009\000\045\000\008\000\000\000\
\\001\000\006\000\019\000\011\000\018\000\015\000\017\000\016\000\016\000\
\\017\000\015\000\018\000\014\000\020\000\013\000\033\000\012\000\
\\042\000\011\000\043\000\010\000\044\000\009\000\045\000\008\000\000\000\
\\001\000\012\000\070\000\013\000\069\000\014\000\068\000\029\000\067\000\
\\033\000\066\000\000\000\
\\001\000\024\000\072\000\000\000\
\\001\000\029\000\084\000\000\000\
\\001\000\030\000\073\000\000\000\
\\001\000\030\000\077\000\037\000\076\000\000\000\
\\001\000\030\000\088\000\037\000\076\000\000\000\
\\001\000\030\000\091\000\000\000\
\\001\000\034\000\061\000\000\000\
\\001\000\034\000\085\000\000\000\
\\001\000\034\000\087\000\037\000\076\000\039\000\086\000\000\000\
\\001\000\034\000\097\000\000\000\
\\001\000\042\000\048\000\000\000\
\\001\000\043\000\049\000\000\000\
\\001\000\046\000\000\000\000\000\
\\099\000\001\000\036\000\019\000\035\000\020\000\034\000\021\000\033\000\
\\022\000\032\000\023\000\031\000\024\000\030\000\025\000\029\000\
\\026\000\028\000\027\000\027\000\028\000\026\000\029\000\025\000\000\000\
\\100\000\000\000\
\\101\000\000\000\
\\102\000\033\000\012\000\042\000\011\000\043\000\010\000\044\000\023\000\
\\045\000\022\000\000\000\
\\103\000\033\000\012\000\042\000\011\000\043\000\010\000\044\000\023\000\
\\045\000\022\000\000\000\
\\104\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\001\000\036\000\019\000\035\000\020\000\034\000\021\000\033\000\
\\022\000\032\000\023\000\031\000\024\000\030\000\025\000\029\000\
\\026\000\028\000\027\000\027\000\028\000\026\000\029\000\025\000\000\000\
\\118\000\001\000\036\000\019\000\035\000\020\000\034\000\021\000\033\000\
\\022\000\032\000\023\000\031\000\024\000\030\000\025\000\029\000\
\\026\000\028\000\027\000\027\000\028\000\026\000\029\000\025\000\000\000\
\\119\000\001\000\036\000\019\000\035\000\020\000\034\000\021\000\033\000\
\\022\000\032\000\023\000\031\000\024\000\030\000\025\000\029\000\
\\026\000\028\000\027\000\027\000\028\000\026\000\029\000\025\000\000\000\
\\120\000\001\000\036\000\019\000\035\000\020\000\034\000\021\000\033\000\
\\022\000\032\000\023\000\031\000\024\000\030\000\025\000\029\000\
\\026\000\028\000\027\000\027\000\028\000\026\000\029\000\025\000\000\000\
\\121\000\001\000\036\000\019\000\035\000\020\000\034\000\021\000\033\000\
\\022\000\032\000\023\000\031\000\024\000\030\000\025\000\029\000\
\\026\000\028\000\027\000\027\000\028\000\026\000\029\000\025\000\000\000\
\\122\000\001\000\036\000\019\000\035\000\020\000\034\000\021\000\033\000\
\\022\000\032\000\023\000\031\000\024\000\030\000\025\000\029\000\
\\026\000\028\000\027\000\027\000\028\000\026\000\029\000\025\000\000\000\
\\123\000\001\000\036\000\019\000\035\000\020\000\034\000\021\000\033\000\
\\022\000\032\000\023\000\031\000\024\000\030\000\025\000\029\000\
\\026\000\028\000\027\000\027\000\028\000\026\000\029\000\025\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\\127\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\\130\000\000\000\
\\131\000\000\000\
\\132\000\000\000\
\\133\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\001\000\036\000\019\000\035\000\020\000\034\000\021\000\033\000\
\\022\000\032\000\023\000\031\000\024\000\030\000\025\000\029\000\
\\026\000\028\000\027\000\027\000\028\000\026\000\029\000\025\000\
\\039\000\062\000\000\000\
\\137\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\000\000\
\\141\000\037\000\076\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\037\000\076\000\039\000\086\000\000\000\
\\147\000\000\000\
\"
val actionRowNumbers =
"\007\000\028\000\025\000\048\000\
\\027\000\024\000\000\000\001\000\
\\054\000\049\000\008\000\009\000\
\\009\000\009\000\009\000\009\000\
\\009\000\009\000\021\000\053\000\
\\055\000\056\000\052\000\022\000\
\\009\000\009\000\009\000\009\000\
\\009\000\009\000\009\000\009\000\
\\009\000\009\000\009\000\017\000\
\\004\000\057\000\010\000\039\000\
\\040\000\044\000\043\000\042\000\
\\041\000\002\000\011\000\013\000\
\\045\000\038\000\037\000\036\000\
\\031\000\035\000\033\000\032\000\
\\030\000\029\000\046\000\051\000\
\\009\000\050\000\061\000\014\000\
\\010\000\010\000\065\000\067\000\
\\066\000\009\000\009\000\047\000\
\\060\000\059\000\010\000\012\000\
\\018\000\019\000\015\000\003\000\
\\006\000\064\000\016\000\062\000\
\\010\000\068\000\063\000\009\000\
\\007\000\020\000\070\000\069\000\
\\034\000\005\000\026\000\058\000\
\\023\000"
val gotoT =
"\
\\001\000\096\000\002\000\005\000\003\000\004\000\004\000\003\000\
\\005\000\002\000\007\000\001\000\000\000\
\\003\000\019\000\004\000\003\000\000\000\
\\000\000\
\\000\000\
\\003\000\022\000\004\000\003\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\036\000\003\000\004\000\004\000\003\000\007\000\001\000\
\\012\000\035\000\000\000\
\\002\000\039\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\002\000\040\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\002\000\041\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\002\000\042\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\002\000\043\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\002\000\044\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\002\000\045\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\048\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\002\000\049\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\002\000\050\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\002\000\051\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\002\000\052\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\002\000\053\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\002\000\054\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\002\000\055\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\002\000\056\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\002\000\057\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\002\000\058\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\063\000\010\000\062\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\073\000\003\000\004\000\004\000\003\000\007\000\001\000\
\\012\000\072\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\077\000\009\000\076\000\010\000\062\000\000\000\
\\008\000\078\000\010\000\062\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\079\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\002\000\080\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\081\000\010\000\062\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\091\000\009\000\090\000\010\000\062\000\000\000\
\\000\000\
\\000\000\
\\002\000\092\000\003\000\004\000\004\000\003\000\007\000\001\000\000\000\
\\001\000\094\000\002\000\093\000\003\000\004\000\004\000\003\000\
\\005\000\002\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 97
val numrules = 52
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | CINT of unit ->  (int) | NAME of unit ->  (string)
 | Comps of unit ->  (expr list)
 | Params of unit ->  ( ( plcType * string )  list)
 | AtomType of unit ->  (plcType) | Types of unit ->  (plcType list)
 | Type of unit ->  (plcType) | AppExpr of unit ->  (expr)
 | TypedVar of unit ->  (plcType*string) | Decl of unit ->  (expr)
 | Const of unit ->  (expr) | AtomExpr of unit ->  (expr)
 | Expr of unit ->  (expr) | Prog of unit ->  (expr)
end
type svalue = MlyValue.svalue
type result = expr
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 45) => true | _ => false
val showTerminal =
fn (T 0) => "SEMIC"
  | (T 1) => "VAR"
  | (T 2) => "FUN"
  | (T 3) => "FUNREC"
  | (T 4) => "COL"
  | (T 5) => "IF"
  | (T 6) => "THEN"
  | (T 7) => "ELSE"
  | (T 8) => "MATCH"
  | (T 9) => "WITH"
  | (T 10) => "EXC"
  | (T 11) => "BOOL"
  | (T 12) => "INT"
  | (T 13) => "NIL"
  | (T 14) => "HEAD"
  | (T 15) => "TAIL"
  | (T 16) => "ISE"
  | (T 17) => "PRINT"
  | (T 18) => "PLUS"
  | (T 19) => "MINUS"
  | (T 20) => "MULTI"
  | (T 21) => "DIV"
  | (T 22) => "AND"
  | (T 23) => "EQ"
  | (T 24) => "NEQ"
  | (T 25) => "LT"
  | (T 26) => "LTE"
  | (T 27) => "CSEQ"
  | (T 28) => "LSB"
  | (T 29) => "RSB"
  | (T 30) => "LB"
  | (T 31) => "RB"
  | (T 32) => "LPAREN"
  | (T 33) => "RPAREN"
  | (T 34) => "FN"
  | (T 35) => "FATARR"
  | (T 36) => "THINARR"
  | (T 37) => "END"
  | (T 38) => "COMMA"
  | (T 39) => "PIPE"
  | (T 40) => "UNDER"
  | (T 41) => "NAME"
  | (T 42) => "CINT"
  | (T 43) => "TRUE"
  | (T 44) => "FALSE"
  | (T 45) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37)
 $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30)
 $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23)
 $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16)
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 
1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Expr Expr1, Expr1left, Expr1right)) :: 
rest671)) => let val  result = MlyValue.Prog (fn _ => let val  (Expr
 as Expr1) = Expr1 ()
 in (Expr)
end)
 in ( LrTable.NT 0, ( result, Expr1left, Expr1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.Decl Decl1, Decl1left, Decl1right)) :: 
rest671)) => let val  result = MlyValue.Prog (fn _ => let val  (Decl
 as Decl1) = Decl1 ()
 in (Decl)
end)
 in ( LrTable.NT 0, ( result, Decl1left, Decl1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.Prog Prog1, _, Prog1right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, _, _)) :: _ :: ( _, ( MlyValue.NAME NAME1, _, _))
 :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = 
MlyValue.Decl (fn _ => let val  (NAME as NAME1) = NAME1 ()
 val  (Expr as Expr1) = Expr1 ()
 val  (Prog as Prog1) = Prog1 ()
 in (Let(NAME, Expr, Prog))
end)
 in ( LrTable.NT 4, ( result, VAR1left, Prog1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.AtomExpr AtomExpr1, AtomExpr1left, 
AtomExpr1right)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (AtomExpr as AtomExpr1) = AtomExpr1 ()
 in (AtomExpr)
end)
 in ( LrTable.NT 1, ( result, AtomExpr1left, AtomExpr1right), rest671)

end
|  ( 4, ( ( _, ( MlyValue.AppExpr AppExpr1, AppExpr1left, 
AppExpr1right)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (AppExpr as AppExpr1) = AppExpr1 ()
 in (AppExpr)
end)
 in ( LrTable.NT 1, ( result, AppExpr1left, AppExpr1right), rest671)

end
|  ( 5, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("+", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("-", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("=", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("*", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("/", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.Expr Expr3, _, Expr3right)) :: _ :: ( _, ( 
MlyValue.Expr Expr2, _, _)) :: _ :: ( _, ( MlyValue.Expr Expr1, _, _))
 :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 val  Expr3 = Expr3 ()
 in (If(Expr1, Expr2, Expr3))
end)
 in ( LrTable.NT 1, ( result, IF1left, Expr3right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("&&", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("!=", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("<", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("<=", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
MINUS1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("-", Expr))
end)
 in ( LrTable.NT 1, ( result, MINUS1left, Expr1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
PRINT1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("print", Expr))
end)
 in ( LrTable.NT 1, ( result, PRINT1left, Expr1right), rest671)
end
|  ( 17, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.Expr (fn _ => (ConB(false)))
 in ( LrTable.NT 1, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 18, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.Expr (fn _ => (ConB(true)))
 in ( LrTable.NT 1, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
EXC1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _ =>
 let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("!", Expr))
end)
 in ( LrTable.NT 1, ( result, EXC1left, Expr1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
HEAD1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("hd", Expr))
end)
 in ( LrTable.NT 1, ( result, HEAD1left, Expr1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
TAIL1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("tl", Expr))
end)
 in ( LrTable.NT 1, ( result, TAIL1left, Expr1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
ISE1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _ =>
 let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("ise", Expr))
end)
 in ( LrTable.NT 1, ( result, ISE1left, Expr1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("::", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2(";", Expr1, Expr2))
end)
 in ( LrTable.NT 1, ( result, Expr1left, Expr2right), rest671)
end
|  ( 25, ( ( _, ( _, _, RSB1right)) :: ( _, ( MlyValue.CINT CINT1, _,
 _)) :: _ :: ( _, ( MlyValue.Expr Expr1, Expr1left, _)) :: rest671))
 => let val  result = MlyValue.Expr (fn _ => let val  (Expr as Expr1)
 = Expr1 ()
 val  (CINT as CINT1) = CINT1 ()
 in (Item(CINT, Expr))
end)
 in ( LrTable.NT 1, ( result, Expr1left, RSB1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.Const Const1, Const1left, Const1right)) :: 
rest671)) => let val  result = MlyValue.AtomExpr (fn _ => let val  (
Const as Const1) = Const1 ()
 in (Const)
end)
 in ( LrTable.NT 2, ( result, Const1left, Const1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.NAME NAME1, NAME1left, NAME1right)) :: 
rest671)) => let val  result = MlyValue.AtomExpr (fn _ => let val  (
NAME as NAME1) = NAME1 ()
 in (Var(NAME))
end)
 in ( LrTable.NT 2, ( result, NAME1left, NAME1right), rest671)
end
|  ( 28, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.Expr Expr1,
 _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.AtomExpr (fn _ => let val  (Expr as Expr1) = Expr1 ()
 in (Expr)
end)
 in ( LrTable.NT 2, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 29, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.Comps Comps1
, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.AtomExpr (fn _ => let val  (Comps as Comps1) = Comps1 ()
 in (List(Comps))
end)
 in ( LrTable.NT 2, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.AtomExpr AtomExpr2, _, AtomExpr2right)) :: 
( _, ( MlyValue.AtomExpr AtomExpr1, AtomExpr1left, _)) :: rest671)) =>
 let val  result = MlyValue.AppExpr (fn _ => let val  AtomExpr1 = 
AtomExpr1 ()
 val  AtomExpr2 = AtomExpr2 ()
 in (Call(AtomExpr1, AtomExpr2))
end)
 in ( LrTable.NT 6, ( result, AtomExpr1left, AtomExpr2right), rest671)

end
|  ( 31, ( ( _, ( MlyValue.AtomExpr AtomExpr1, _, AtomExpr1right)) :: 
( _, ( MlyValue.AppExpr AppExpr1, AppExpr1left, _)) :: rest671)) =>
 let val  result = MlyValue.AppExpr (fn _ => let val  (AppExpr as 
AppExpr1) = AppExpr1 ()
 val  (AtomExpr as AtomExpr1) = AtomExpr1 ()
 in (Call(AppExpr, AtomExpr))
end)
 in ( LrTable.NT 6, ( result, AppExpr1left, AtomExpr1right), rest671)

end
|  ( 32, ( ( _, ( MlyValue.CINT CINT1, CINT1left, CINT1right)) :: 
rest671)) => let val  result = MlyValue.Const (fn _ => let val  (CINT
 as CINT1) = CINT1 ()
 in (ConI(CINT))
end)
 in ( LrTable.NT 3, ( result, CINT1left, CINT1right), rest671)
end
|  ( 33, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.Const (fn _ => (ConB(false)))
 in ( LrTable.NT 3, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 34, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.Const (fn _ => (ConB(true)))
 in ( LrTable.NT 3, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 35, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( _, LPAREN1left, _))
 :: rest671)) => let val  result = MlyValue.Const (fn _ => (List([])))
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 36, ( ( _, ( _, _, RPAREN1right)) :: _ :: _ :: _ :: ( _, ( 
MlyValue.Type Type1, _, _)) :: _ :: ( _, ( _, LPAREN1left, _)) :: 
rest671)) => let val  result = MlyValue.Const (fn _ => let val  (Type
 as Type1) = Type1 ()
 in (ESeq(SeqT(Type)))
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Comps (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Expr1::Expr2::[])
end)
 in ( LrTable.NT 11, ( result, Expr1left, Expr2right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.Comps Comps1, _, Comps1right)) :: _ :: ( _,
 ( MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result
 = MlyValue.Comps (fn _ => let val  (Expr as Expr1) = Expr1 ()
 val  (Comps as Comps1) = Comps1 ()
 in (Expr::Comps)
end)
 in ( LrTable.NT 11, ( result, Expr1left, Comps1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.AtomType AtomType1, AtomType1left, 
AtomType1right)) :: rest671)) => let val  result = MlyValue.Type (fn _
 => let val  (AtomType as AtomType1) = AtomType1 ()
 in (AtomType)
end)
 in ( LrTable.NT 7, ( result, AtomType1left, AtomType1right), rest671)

end
|  ( 40, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.Types Types1
, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.Type (fn _ => let val  (Types as Types1) = Types1 ()
 in (ListT(Types))
end)
 in ( LrTable.NT 7, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 41, ( ( _, ( _, _, RSB1right)) :: ( _, ( MlyValue.Type Type1, _,
 _)) :: ( _, ( _, LSB1left, _)) :: rest671)) => let val  result = 
MlyValue.Type (fn _ => let val  (Type as Type1) = Type1 ()
 in (SeqT(Type))
end)
 in ( LrTable.NT 7, ( result, LSB1left, RSB1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.Type Type2, _, Type2right)) :: _ :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.Type (fn _ => let val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 in (FunT(Type1, Type2))
end)
 in ( LrTable.NT 7, ( result, Type1left, Type2right), rest671)
end
|  ( 43, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.AtomType (fn _ => (ListT([])))
 in ( LrTable.NT 9, ( result, NIL1left, NIL1right), rest671)
end
|  ( 44, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.AtomType (fn _ => (BoolT))
 in ( LrTable.NT 9, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 45, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.AtomType (fn _ => (IntT))
 in ( LrTable.NT 9, ( result, INT1left, INT1right), rest671)
end
|  ( 46, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.Type Type1,
 _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.AtomType (fn _ => let val  (Type as Type1) = Type1 ()
 in (Type)
end)
 in ( LrTable.NT 9, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.Type Type2, _, Type2right)) :: _ :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.Types (fn _ => let val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 in (Type1::Type2::[])
end)
 in ( LrTable.NT 8, ( result, Type1left, Type2right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.Types Types1, _, Types1right)) :: _ :: ( _,
 ( MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result
 = MlyValue.Types (fn _ => let val  (Type as Type1) = Type1 ()
 val  (Types as Types1) = Types1 ()
 in (Type::Types)
end)
 in ( LrTable.NT 8, ( result, Type1left, Types1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.NAME NAME1, _, NAME1right)) :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.TypedVar (fn _ => let val  (Type as Type1) = Type1 ()
 val  (NAME as NAME1) = NAME1 ()
 in (Type, NAME)
end)
 in ( LrTable.NT 5, ( result, Type1left, NAME1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.TypedVar TypedVar1, TypedVar1left, 
TypedVar1right)) :: rest671)) => let val  result = MlyValue.Params (fn
 _ => let val  (TypedVar as TypedVar1) = TypedVar1 ()
 in (TypedVar::[])
end)
 in ( LrTable.NT 10, ( result, TypedVar1left, TypedVar1right), rest671
)
end
|  ( 51, ( ( _, ( MlyValue.Params Params1, _, Params1right)) :: _ :: (
 _, ( MlyValue.TypedVar TypedVar1, TypedVar1left, _)) :: rest671)) =>
 let val  result = MlyValue.Params (fn _ => let val  (TypedVar as 
TypedVar1) = TypedVar1 ()
 val  (Params as Params1) = Params1 ()
 in (TypedVar::Params)
end)
 in ( LrTable.NT 10, ( result, TypedVar1left, Params1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : PlcParser_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun SEMIC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNREC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun COL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun MATCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun WITH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun EXC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun HEAD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun TAIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun ISE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun MULTI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun LTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun CSEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun LSB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun RSB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun LB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun RB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun FATARR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun THINARR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun PIPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun UNDER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun NAME (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.NAME (fn () => i),p1,p2))
fun CINT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.CINT (fn () => i),p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
end
end
