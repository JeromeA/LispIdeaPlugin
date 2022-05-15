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
    LispSexp arg = form.getSexpList().get(1);
    String stringDesignator = getStringDesignator(arg, analyzer.annotations, analyzer.packageManager);
    if (stringDesignator == null) {
      analyzer.annotations.highlightError(arg, "Expected name designator");
      return;
    }
    LispPackage newPackage = analyzer.packageManager.getPackage(stringDesignator);
    if (newPackage == null) {
      analyzer.annotations.highlightUnknown(arg, String.format("Unknown package '%s'", stringDesignator));
      return;
    }
    analyzer.packageManager.setCurrentPackage(newPackage);
  }

}
