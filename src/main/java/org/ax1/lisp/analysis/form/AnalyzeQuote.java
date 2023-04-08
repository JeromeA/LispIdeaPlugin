package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

public class AnalyzeQuote {

  public void analyze(AnalysisContext context, LispSexp form) {
    analyzeForm(context, form);
  }

  private void analyzeForm(AnalysisContext context, LispSexp form) {
    LispList list = form.getList();
    if (list != null) {
      context.highlighter.highlightConstant(list.getFirstChild());
      context.highlighter.highlightConstant(list.getLastChild());
      context.analyzer.analyzeReaderExpressions(list);
      list.getSexpList().forEach(sexp -> analyzeForm(context, sexp));
    } else {
      context.highlighter.highlightConstant(form);
    }
  }
}
