package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

public class AnalyzeEcase implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    List<LispSexp> sexpList = form.getSexpList();
    if (sexpList.size() < 2) {
      context.highlighter.highlightError(form, "ECASE needs at least 1 argument");
      return;
    }
    context.analyzer.analyzeForm(sexpList.get(1));
    sexpList.stream().skip(2).forEach(sexp -> {
      LispList list = sexp.getList();
      if (list == null || list.getSexpList().size() < 2) {
        context.highlighter.highlightError(sexp, "key-form clause expected");
      } else {
        context.highlighter.highlightConstant(list.getSexpList().get(0));
        context.analyzer.analyzeForms(list.getSexpList(), 1);
      }
    });
  }
}
