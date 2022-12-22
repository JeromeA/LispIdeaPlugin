package org.ax1.lisp;

import com.intellij.codeInsight.editorActions.SimpleTokenSetQuoteHandler;
import org.ax1.lisp.psi.LispTypes;

public class LispQuoteHandler extends SimpleTokenSetQuoteHandler {

  public LispQuoteHandler() {
    super(LispTypes.STRING_QUOTE);
  }
}
