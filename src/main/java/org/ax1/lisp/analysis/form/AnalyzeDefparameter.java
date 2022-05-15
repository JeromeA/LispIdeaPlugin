package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;

public class AnalyzeDefparameter implements FormAnalyzer {

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.annotations.highlightKeyword(form);
    List<LispSexp> sexpList = form.getSexpList();
    if (sexpList.size() < 3) {
      analyzer.annotations.highlightError(form, "DEFPARAMETER takes at least 2 arguments");
      return;
    }
    LispSymbol symbol = sexpList.get(1).getSymbol();
    if (symbol == null) {
      analyzer.annotations.highlightError(sexpList.get(1), "Variable name expected");
      return;
    }
    analyzer.packageManager.getVariable(symbol.getText()).setDefinition(form, symbol);
    analyzer.analyzeForm(sexpList.get(2));
  }
}
