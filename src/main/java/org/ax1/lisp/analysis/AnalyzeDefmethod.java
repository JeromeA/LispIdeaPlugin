package org.ax1.lisp.analysis;

import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;

public class AnalyzeDefmethod implements Analyzer {

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.annotations.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      analyzer.annotations.highlightError(form, "DEFMETHOD needs at least 2 arguments.");
      return;
    }
    LispSexp sexp1 = list.get(1);
    LispSymbol symbol1 = sexp1.getSymbol();
    if (symbol1 == null) {
      analyzer.annotations.highlightError(sexp1, "Function name expected");
      return;
    }
    int arg = 2;
    // Skip method qualifiers.
    while (arg < list.size() && list.get(arg).getList() == null) arg++;
    if (arg == list.size()) {
      analyzer.annotations.highlightError(form, "Missing lambda list");
      return;
    }
    analyzeLambdaList(analyzer, list.get(arg++));
    while(arg < list.size()) {
      analyzer.analyzeForm(list.get(arg));
      arg++;
    }
  }

  private void analyzeLambdaList(SyntaxAnalyzer analyzer, LispSexp lambdaList) {
    // TODO
  }
}
