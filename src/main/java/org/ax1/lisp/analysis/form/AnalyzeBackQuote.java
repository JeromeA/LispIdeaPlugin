package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalyzerContext;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispQuoted;
import org.ax1.lisp.psi.LispSexp;

import static org.ax1.lisp.analysis.BaseLispElement.Type.DATA;

public class AnalyzeBackQuote {

  // We arbitrarily decide to highlight QUOTE and BACKQUOTE expressions as data.
  // TODO: find a nice heuristic that tells us that it's likely to be code.
  //  Possible heuristics:
  //  - this is the top-level sexp of a macro (this is a very strong one).
  //  - the first element of the list is a standard common lisp function.
  //  - the first element of the list is a known function (this one is tricky, because we are in the middle of
  //    the pass in charge of finding all the known functions).
  public void analyze(AnalyzerContext context, LispSexp form) {
    analyzeForm(context, form);
  }

  private void analyzeForm(AnalyzerContext context, LispSexp form) {
    form.setType(DATA);
    LispList list = form.getList();
    if (list != null) {
      list.getSexpList().forEach(sexp -> analyzeSexp(context, sexp));
    }
  }

  private void analyzeSexp(AnalyzerContext context, LispSexp sexp) {
    LispQuoted quoted = sexp.getQuoted();
    if (quoted == null) {
      analyzeForm(context, sexp);
      return;
    }
    String quoteType = quoted.getFirstChild().getText();
    if (quoteType.equals(",") || quoteType.equals(",@")) {
      SyntaxAnalyzer.INSTANCE.analyzeForm(context, quoted.getSexp());
    }
  }

}
