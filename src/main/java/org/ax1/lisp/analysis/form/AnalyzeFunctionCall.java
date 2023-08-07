package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;

import java.util.Set;

import static org.ax1.lisp.analysis.BaseLispElement.Type.KEYWORD;


public class AnalyzeFunctionCall implements FormAnalyzer {

  /**
   * Names that could look like function calls, but behave effectively like language keywords and should
   * be highlighted as such.
   */
  private static final Set<String> KEYWORDS =
      Set.of("IF", "IGNORE", "RETURN", "SETQ", "SPECIAL", "UNLESS", "WHEN");

  @Override
  public void analyze(LispList form) {
    LispSexp functionName = form.getSexpList().get(0);
    LispSymbolName symbolName = functionName.getSymbolName();
    if (KEYWORDS.contains(symbolName.getValue())) {
      symbolName.setType(KEYWORD);

    }
    SyntaxAnalyzer.INSTANCE.analyzeForms(form.getSexpList(), 1);
  }
}
