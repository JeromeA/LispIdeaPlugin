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
white_space = [\ \r\n\t]
comment = ; [^\r\n]* {eol}?
number = -? [0-9]+ | "#x" [0-9a-fA-F]+
character = "#\\" [^\ \r\n\t]
quote = ['`] | "," "@"?
double_quote = \"
symbol = [^\ \r\n\t\"'`,;()0-9] [^\ \r\n\t\"';()]*

%state STRING

%%

<YYINITIAL> {
  {lparen}                       { return LispTypes.LPAREN; }
  {rparen}                       { return LispTypes.RPAREN; }
  {comment}                      { return LispTypes.COMMENT; }
  {number}                       { return LispTypes.NUMBER; }
  {character}                    { return LispTypes.CHARACTER; }
  {quote}                        { return LispTypes.QUOTE; }
  {double_quote}                 { yybegin(STRING); }
  {white_space}+                 { return TokenType.WHITE_SPACE; }
  {symbol}                       { return LispTypes.SYMBOL_TOKEN; }
}

<STRING> {
  {double_quote}                 { yybegin(YYINITIAL); return LispTypes.STRING; }
  [^\"]+                         { }
  <<EOF>>                        { yybegin(YYINITIAL); return LispTypes.STRING; }
}
