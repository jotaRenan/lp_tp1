(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

fun keyword (s, lpos, rpos) = 
    case s of  
        "Bool" => BOOL (lpos, rpos)
        | "else" => ELSE (lpos, rpos)
        | "end" => END (lpos, rpos)
        | "false" => FALSE (lpos, rpos)
        | "fn" => FN (lpos, rpos)
        | "fun" => FUN (lpos, rpos)
        | "hd" => HEAD(lpos, rpos)
        | "if" => IF (lpos, rpos)
        | "Int" => INT (lpos, rpos)
        | "ise" => ISE (lpos, rpos)
        | "match" => MATCH (lpos, rpos)
        | "Nil" => NIL (lpos, rpos)
        | "print" => PRINT (lpos, rpos)
        | "rec" => FUNREC (lpos, rpos)
        | "then" => THEN (lpos, rpos)
        | "tl" => TAIL (lpos, rpos)
        | "true" => TRUE(lpos, rpos)
        | "var" => VAR (lpos, rpos)
        | "with" => WITH (lpos, rpos)
        | "_" => UNDER (lpos, rpos)
        | _ => NAME (s, lpos, rpos)

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

fun strToInt s = 
    case Int.fromString s of
        SOME i => i
        | NONE => raise Fail("Could not convert string '" ^ s ^ "' to integer")

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
whitespace=[\ \t];
identifier=[a-zA-Z_][a-zA-Z_0-9]*;
%%

\n => (lineNumber := !lineNumber +1; lex());
{whitespace}+ => (lex());
{digit}+ => (CINT(strToInt(yytext), yypos, yypos));
{identifier} => (keyword(yytext, yypos, yypos));
"+" => (PLUS(yypos, yypos));
"-" => (MINUS(yypos, yypos));
"*" => (MULTI(yypos, yypos));
"/" => (DIV(yypos, yypos));
";" => (SEMIC(yypos, yypos));
"::" => (CSEQ(yypos, yypos));
"=" => (EQ(yypos, yypos));
"(" => (LPAREN(yypos, yypos));
")" => (RPAREN(yypos, yypos));
"[" => (LSB(yypos, yypos));
"]" => (RSB(yypos, yypos));
"{" => (LB(yypos, yypos));
"}" => (RB(yypos, yypos));
"&&" => (AND(yypos, yypos));
"!=" => (NEQ(yypos, yypos));
"<" => (LT(yypos, yypos));
"<=" => (LTE(yypos, yypos));
"!" => (EXC(yypos, yypos));
"," => (COMMA(yypos, yypos));
"|" => (PIPE(yypos, yypos));
"->" => (THINARR(yypos, yypos));
"=>" => (FATARR(yypos, yypos));
":" => (COL(yypos, yypos));
"_" => (UNDER(yypos, yypos));
. => (error("\n***Lexer error: bad character ***\n"); raise Fail("Lexer error: bad character "^yytext));
