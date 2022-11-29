package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;
import java.util.stream.Collectors;

public class AnalyzeCond implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    List<LispSexp> sexpList = form.getSexpList();
    for (LispSexp sexp : sexpList.stream().skip(1).collect(Collectors.toList())) {
      LispList pair = sexp.getList();
      if (pair == null || pair.getSexpList().size() < 2) {
        context.highlighter.highlightError(sexp, "(test form*) expected");
        continue;
      }
      context.analyzer.analyzeForms(pair.getSexpList(), 0);
    }
  }
}
