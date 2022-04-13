package org.ax1.lisp.analysis;

import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;
import java.util.stream.Collectors;

public class AnalyzeCond implements Analyzer{
  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.highlightKeyword(form);
    List<LispSexp> sexpList = form.getSexpList();
    for (LispSexp sexp : sexpList.stream().skip(1).collect(Collectors.toList())) {
      LispList pair = sexp.getList();
      if (pair == null || pair.getSexpList().size() != 2) {
        analyzer.highlightError(sexp, "(test-form form) pair expected");
        continue;
      }
      analyzer.analyzeForm(pair.getSexpList().get(0));
      analyzer.analyzeForm(pair.getSexpList().get(1));
    }
  }
}
