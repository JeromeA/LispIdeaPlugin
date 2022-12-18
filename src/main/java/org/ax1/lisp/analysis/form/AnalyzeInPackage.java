package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import static org.ax1.lisp.analysis.Strings.getStringDesignator;

public class AnalyzeInPackage implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    if (form.getSexpList().size() != 2) {
      context.highlighter.highlightError(form, "IN-PACKAGE needs exactly 1 argument");
      return;
    }
    LispSexp sexp1 = form.getSexpList().get(1);
    String packageName = getStringDesignator(context, sexp1);
    if (packageName == null) {
      context.highlighter.highlightError(sexp1, "Expected name designator");
      return;
    }
    context.result.addInPackage(packageName, sexp1);
    context.setCurrentPackage(packageName);
  }
}
