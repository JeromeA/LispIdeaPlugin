package org.ax1.lisp.parsing;

import com.intellij.lexer.FlexAdapter;

public class LispLexerAdapter extends FlexAdapter {

  public LispLexerAdapter() {
    super(new LispLexer(null));
  }

}
