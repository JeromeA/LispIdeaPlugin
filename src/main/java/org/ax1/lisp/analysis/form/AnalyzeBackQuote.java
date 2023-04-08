package org.ax1.lisp.analysis.form;

import com.intellij.psi.PsiElement;
import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispQuoted;
import org.ax1.lisp.psi.LispSexp;

public class AnalyzeBackQuote {

  // We arbitrarily decide to highlight QUOTE and BACKQUOTE expressions as data.
  // TODO: find a nice heuristic that tells us that it's likely to be code.
  //  Possible heuristics:
  //  - this is the top-level sexp of a macro (this is a very strong one).
  //  - the first element of the list is a standard common lisp function.
  //  - the first element of the list is a known function (this one is tricky, because we are in the middle of
  //    the pass in charge of finding all the known functions).
  public void analyze(AnalysisContext context, LispSexp form) {
    analyzeForm(context, form);
  }

  private void analyzeForm(AnalysisContext context, LispSexp form) {
    LispList list = form.getList();
    if (list != null) {
      context.highlighter.highlightConstant(list.getFirstChild());
      context.highlighter.highlightConstant(list.getLastChild());
      list.getSexpList().forEach(sexp -> analyzeSexp(context, sexp));
    } else {
      context.highlighter.highlightConstant(form);
    }
  }

  private void analyzeSexp(AnalysisContext context, LispSexp sexp) {
    LispQuoted quoted = sexp.getQuoted();
    if (quoted == null) {
      analyzeForm(context, sexp);
      return;
    }
    PsiElement quote = quoted.getFirstChild();
    context.highlighter.highlightKeyword(quote);
    String quoteType = quote.getText();
    if (quoteType.equals(",") || quoteType.equals(",@")) {
      context.analyzer.analyzeForm(quoted.getSexp());
    }
  }

}
