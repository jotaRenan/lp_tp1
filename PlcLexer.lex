(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

fun keyword (s, lpos, rpos) = 
    case s of  
        "var" => VAR (lpos, rpos)
        | "fun" => FUN (lpos, rpos)
        | "rec" => FUNREC (lpos, rpos)
        | "if" => IF (lpos, rpos)
        | "then" => THEN (lpos, rpos)
        | "else" => ELSE (lpos, rpos)
        | "print" => PRINT (lpos, rpos)
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
"=" => (EQ(yypos, yypos));
"(" => (LPAREN(yypos, yypos));
")" => (RPAREN(yypos, yypos));
. => (error("error"); raise Fail("error"));