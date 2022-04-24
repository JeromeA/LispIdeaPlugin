package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.Package;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

public class AnalyzeInPackage implements Analyzer {
  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.highlightKeyword(form);
    if (form.getSexpList().size() != 2) {
      analyzer.highlightError(form, "IN-PACKAGE needs exactly 1 argument");
      return;
    }
    LispSexp arg = form.getSexpList().get(1);
    String stringDesignator = analyzer.getStringDesignator(arg);
    if (stringDesignator == null) {
      analyzer.highlightError(arg, "Expected name designator");
      return;
    }
    Package newPackage = analyzer.symbolManager.getPackage(stringDesignator);
    if (newPackage == null) {
      analyzer.highlightUnknown(arg, String.format("Unknown package '%s'", stringDesignator));
      return;
    }
    analyzer.symbolManager.setCurrentPackage(newPackage);
  }

}
