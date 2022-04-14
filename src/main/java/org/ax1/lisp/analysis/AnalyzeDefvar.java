package org.ax1.lisp.analysis;

import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;

public class AnalyzeDefvar implements Analyzer {

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.highlightKeyword(form);
    List<LispSexp> sexpList = form.getSexpList();
    if (sexpList.size() < 2) {
      analyzer.highlightError(form, "DEFVAR takes at least one argument");
      return;
    }
    LispSymbol symbol = sexpList.get(1).getSymbol();
    if (symbol == null) {
      analyzer.highlightError(sexpList.get(1), "Variable name expected");
      return;
    }
    analyzer.variables.registerSpecialDefinition(form, symbol);
    if (sexpList.size() < 3) return;
    analyzer.analyzeForm(sexpList.get(2));
  }
}
