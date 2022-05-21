package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LispPackage;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import static org.ax1.lisp.analysis.StringDesignator.getStringDesignator;

public class AnalyzeInPackage implements FormAnalyzer {
  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.annotations.highlightKeyword(form);
    if (form.getSexpList().size() != 2) {
      analyzer.annotations.highlightError(form, "IN-PACKAGE needs exactly 1 argument");
      return;
    }
    LispSexp sexp1 = form.getSexpList().get(1);
    String stringDesignator = getStringDesignator(sexp1, analyzer.annotations, analyzer.packageManager);
    if (stringDesignator == null) {
      analyzer.annotations.highlightError(sexp1, "Expected name designator");
      return;
    }
    LispPackage aPackage = analyzer.packageManager.getPackage(stringDesignator);
    if (aPackage == null) {
      analyzer.annotations.highlightUnknown(sexp1, String.format("Unknown package '%s'", stringDesignator));
      return;
    }
    if (sexp1.getSymbol() != null) {
      aPackage.getDefinition().addUsage(sexp1.getSymbol());
    }
    analyzer.packageManager.setCurrentPackage(aPackage);
  }

}
