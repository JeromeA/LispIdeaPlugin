package org.ax1.lisp.parsing;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.HighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import org.ax1.lisp.psi.LispTypes;
import org.jetbrains.annotations.NotNull;

import static com.intellij.openapi.editor.colors.TextAttributesKey.createTextAttributesKey;

public class LispSyntaxHighlighter extends SyntaxHighlighterBase {

  public static final TextAttributesKey STRING =
      createTextAttributesKey("LISP_STRING", DefaultLanguageHighlighterColors.STRING);
  public static final TextAttributesKey COMMENT =
      createTextAttributesKey("LISP_COMMENT", DefaultLanguageHighlighterColors.LINE_COMMENT);
  public static final TextAttributesKey BAD_CHARACTER =
      createTextAttributesKey("LISP_BAD_CHARACTER", HighlighterColors.BAD_CHARACTER);
  public static final TextAttributesKey KEYWORD =
      createTextAttributesKey("LISP_KEYWORD", DefaultLanguageHighlighterColors.KEYWORD);
  public static final TextAttributesKey NUMBER =
      createTextAttributesKey("LISP_NUMBER", DefaultLanguageHighlighterColors.NUMBER);
  public static final TextAttributesKey READER_MACRO =
      createTextAttributesKey("READER_MACRO", DefaultLanguageHighlighterColors.METADATA);

  private static final TextAttributesKey[] BAD_CHAR_KEYS = new TextAttributesKey[]{BAD_CHARACTER};
  private static final TextAttributesKey[] STRING_KEYS = new TextAttributesKey[]{STRING};
  private static final TextAttributesKey[] COMMENT_KEYS = new TextAttributesKey[]{COMMENT};
  private static final TextAttributesKey[] KEYWORD_KEYS = new TextAttributesKey[]{KEYWORD};
  private static final TextAttributesKey[] NUMBER_KEYS = new TextAttributesKey[]{NUMBER};
  private static final TextAttributesKey[] READER_MACRO_KEYS = new TextAttributesKey[]{READER_MACRO};
  private static final TextAttributesKey[] EMPTY_KEYS = new TextAttributesKey[0];

  @NotNull
  @Override
  public Lexer getHighlightingLexer() {
    return new LispLexerAdapter();
  }

  @Override
  public TextAttributesKey @NotNull [] getTokenHighlights(IElementType tokenType) {
    if (tokenType.equals(LispTypes.NUMBER)) {
      return NUMBER_KEYS;
    }
    if (tokenType.equals(LispTypes.CHARACTER)) {
      return NUMBER_KEYS;
    }
    if (tokenType.equals(LispTypes.STRING_CONTENT_TOKEN) || tokenType.equals(LispTypes.STRING_QUOTE)) {
      return STRING_KEYS;
    }
    if (tokenType.equals(LispTypes.COMMENT)) {
      return COMMENT_KEYS;
    }
    if (tokenType.equals(LispTypes.SHARP_REF)) {
      return READER_MACRO_KEYS;
    }
    if (tokenType.equals(TokenType.BAD_CHARACTER) || tokenType.equals(LispTypes.SHARP_UNSUPPORTED)) {
      return BAD_CHAR_KEYS;
    }
    if (tokenType.equals(LispTypes.QUOTE)) {
      return KEYWORD_KEYS;
    }
    return EMPTY_KEYS;
  }

}
