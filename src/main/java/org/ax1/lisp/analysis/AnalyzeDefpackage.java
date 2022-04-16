package org.ax1.lisp.analysis;

import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;

public class AnalyzeDefpackage implements Analyzer {

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.highlightKeyword(form);
    List<LispSexp> formList = form.getSexpList();
    if (formList.size() < 2) {
      analyzer.highlightError(form, "DEFPACKAGE needs at least 1 argument");
      return;
    }
    String packageName = analyzer.getStringDesignator(formList.get(1));
    if (packageName == null) {
      analyzer.highlightError(formList.get(1), "Package name (as a string designator) expected");
      return;
    }
    Package newPackage = new Package(packageName);
    formList.stream().skip(2).forEach(sexp -> analyzeOption(analyzer, sexp, newPackage));
    analyzer.packages.add(newPackage);
  }

  private void analyzeOption(SyntaxAnalyzer analyzer, LispSexp sexp, Package newPackage) {
    LispList lispList = sexp.getList();
    if (lispList == null || lispList.getSexpList().size() < 1) {
      analyzer.highlightError(sexp, "option (as a list) expected");
      return;
    }
    List<LispSexp> list = lispList.getSexpList();
    LispSymbol optionSymbol = list.get(0).getSymbol();
    if (optionSymbol == null) {
      analyzer.highlightError(lispList.getSexpList().get(0), "option name expected");
      return;
    }
    analyzer.highlightKeyword(optionSymbol);
    switch(optionSymbol.getText()) {
      case ":use":
        analyzeOptionUses(analyzer, newPackage, list);
        break;
      default:
        analyzer.highlightError(optionSymbol, "option name expected");
    }
  }

  private void analyzeOptionUses(SyntaxAnalyzer analyzer, Package newPackage, List<LispSexp> list) {
    for (int i = 1; i < list.size(); i++) {
      LispSexp sexp = list.get(i);
      String packageName = analyzer.getStringDesignator(sexp);
      if (packageName == null) {
        analyzer.highlightError(sexp, "package name (string designator) expected");
      } else {
        Package aPackage = analyzer.packages.get(packageName);
        if (aPackage == null) {
          analyzer.highlightError(sexp, "unknown package");
        } else {
          newPackage.addUse(aPackage);
        }
      }
    }

  }
}
