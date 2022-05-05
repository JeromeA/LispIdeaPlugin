package org.ax1.lisp.analysis;

import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;

import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.FUNCTION_DECLARATION;
import static org.ax1.lisp.psi.LispTypes.STRING;

public class AnalyzeDefstruct implements Analyzer {

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.annotations.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      analyzer.annotations.highlightError(form, "DEFSTRUCT needs at least 1 argument");
      return;
    }
    LispSymbol symbol = list.get(1).getSymbol();
    if (symbol == null) {
      analyzer.annotations.highlightError(list.get(1), "Struct name expected");
      return;
    }
    String structName = symbol.getText();
    analyzer.annotations.highlight(symbol, FUNCTION_DECLARATION);

    // Skip documentation.
    int arg = 2;
    if (list.size() > arg && list.get(arg).getFirstChild().getNode().getElementType() == STRING) arg++;

    String constructorName = structName;
    while (list.size() > arg) {
      arg++;
    }

    analyzer.symbolManager.getFunction("make-" + constructorName).setDefinition(form, symbol);
  }
}
