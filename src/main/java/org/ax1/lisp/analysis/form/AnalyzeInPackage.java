package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.impl.LispStringDesignator;

import java.util.Locale;
import java.util.function.Function;

import static org.ax1.lisp.analysis.SyntaxAnalyzer.getCompletionPrefix;
import static org.ax1.lisp.analysis.SyntaxAnalyzer.isCompletion;

public class AnalyzeInPackage implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    if (form.getSexpList().size() != 2) {
      context.highlighter.highlightError(form, "IN-PACKAGE needs exactly 1 argument");
      return;
    }
    LispSexp sexp1 = form.getSexpList().get(1);
    LispStringDesignator stringDesignator = sexp1.getStringDesignator();
    if (stringDesignator == null) {
      context.highlighter.highlightError(sexp1, "Expected name designator");
      return;
    }

    context.result.addPackageUsage(stringDesignator);
    context.setCurrentPackage(stringDesignator.getValue());
  }
}
