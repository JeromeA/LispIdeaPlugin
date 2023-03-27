package org.ax1.lisp.parsing;

import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;
import org.ax1.lisp.psi.LispTypes;
import com.intellij.psi.TokenType;

%%

%class LispLexer
%implements FlexLexer
%unicode
%function advance
%type IElementType
%eof{  return;
%eof}
//%debug

lparen = "("
rparen = ")"
eol = \r | \n | \r\n
white_space = [\ \r\n\t\f]
comment = ; [^\r\n]* {eol}?
reader_comment = "#|" [^|]* "|#"
sharp_plus = "#" [+-]
sharp_ref = "#" [0-9]+ "#"
sharp_prefix = "#" [0-9]* [=.:o]
sharp_unsupported = "#" [0-9]* [^\ \r\n\t\f]
number = -? [0-9]+ ("." [0-9]*)? | "#x" [0-9a-fA-F]+
character = "#\\" [^\ \r\n\t\f][a-zA-Z]*
quote = ['`] | "," "@"? | "#'"
double_quote = \"
symbol = [^\ \r\n\t\f\"'`,;():#] [^\ \r\n\t\f\"'`,;():]*
colon = ::?

%state STRING

%%

<YYINITIAL> {
  {lparen}                       { return LispTypes.LPAREN; }
  {rparen}                       { return LispTypes.RPAREN; }
  {comment}                      { return LispTypes.COMMENT; }
  {reader_comment}               { return LispTypes.COMMENT; }
  {number}                       { return LispTypes.NUMBER; }
  {quote}                        { return LispTypes.QUOTE; }
  {double_quote}                 { yybegin(STRING); return LispTypes.STRING_QUOTE; }
  {white_space}+                 { return TokenType.WHITE_SPACE; }
  {character}                    { return LispTypes.CHARACTER; }
  {sharp_ref}                    { return LispTypes.SHARP_REF; }
  {sharp_plus}                   { return LispTypes.SHARP_PLUS; }
  {sharp_prefix}                 { return LispTypes.SHARP_PREFIX; }
  {sharp_unsupported}            { return LispTypes.SHARP_UNSUPPORTED; }
  {symbol} / :                   { return LispTypes.PACKAGE_TOKEN; }
  {colon}                        { return LispTypes.COLON_TOKEN; }
  {symbol}                       { return LispTypes.SYMBOL_TOKEN; }
}

<STRING> {
  {double_quote}                 { yybegin(YYINITIAL); return LispTypes.STRING_QUOTE; }
  ([^\"\\] | \\.)+               { return LispTypes.STRING_CONTENT_TOKEN; }
  <<EOF>>                        { yybegin(YYINITIAL); return LispTypes.STRING_QUOTE; }
}
