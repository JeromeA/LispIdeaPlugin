package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.FUNCTION_DECLARATION;

public class AnalyzeDefgeneric implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      context.highlighter.highlightError(form, "DEFGENERIC needs at least 2 arguments");
      return;
    }
    LispSexp sexp1 = list.get(1);
    if (sexp1.getSymbol() == null) {
      context.highlighter.highlightError(sexp1, "Function name expected");
      return;
    }
    context.addFunctionDefinition(sexp1.getSymbol());
    context.highlighter.highlight(sexp1, FUNCTION_DECLARATION);
  }
}
