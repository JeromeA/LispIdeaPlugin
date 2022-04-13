package org.ax1.lisp.analysis;

import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

public class AnalyzeLoop implements Analyzer {

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyze(analyzer, form, 1);
  }

  private void analyze(SyntaxAnalyzer analyzer, LispList form, int startAt) {
    analyzer.highlightKeyword(form);
    List<LispSexp> formList = form.getSexpList();
    if (formList.size() <= startAt) return;

  }
}
