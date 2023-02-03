package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;
import java.util.stream.Collectors;

public class AnalyzeCond implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    List<LispSexp> sexpList = form.getSexpList();
    for (LispSexp sexp : sexpList.stream().skip(1).collect(Collectors.toList())) {
      LispList condCase = sexp.getList();
      if (condCase == null || condCase.getSexpList().isEmpty()) {
        context.highlighter.highlightError(sexp, "(test form*) expected");
        continue;
      }
      context.analyzer.analyzeForms(condCase.getSexpList(), 0);
    }
  }
}
