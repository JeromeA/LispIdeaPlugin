package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.Package;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;

import static org.ax1.lisp.analysis.StringDesignator.getStringDesignator;

public class AnalyzeDefpackage implements Analyzer {

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.annotations.highlightKeyword(form);
    List<LispSexp> formList = form.getSexpList();
    if (formList.size() < 2) {
      analyzer.annotations.highlightError(form, "DEFPACKAGE needs at least 1 argument");
      return;
    }
    String packageName = getStringDesignator(formList.get(1), analyzer.annotations, analyzer.symbolManager);
    if (packageName == null) {
      analyzer.annotations.highlightError(formList.get(1), "Package name (as a string designator) expected");
      return;
    }
    Package newPackage = new Package(packageName);
    formList.stream().skip(2).forEach(sexp -> analyzeOption(analyzer, sexp, newPackage));
    analyzer.symbolManager.add(newPackage);
  }

  private void analyzeOption(SyntaxAnalyzer analyzer, LispSexp sexp, Package newPackage) {
    LispList lispList = sexp.getList();
    if (lispList == null || lispList.getSexpList().size() < 1) {
      analyzer.annotations.highlightError(sexp, "option (as a list) expected");
      return;
    }
    List<LispSexp> list = lispList.getSexpList();
    LispSymbol optionSymbol = list.get(0).getSymbol();
    if (optionSymbol == null) {
      analyzer.annotations.highlightError(lispList.getSexpList().get(0), "option name expected");
      return;
    }
    analyzer.annotations.highlightKeyword(optionSymbol);
    switch(optionSymbol.getText()) {
      case ":use":
        analyzeOptionUses(analyzer, newPackage, list);
        break;
      default:
        analyzer.annotations.highlightError(optionSymbol, "option name expected");
    }
  }

  private void analyzeOptionUses(SyntaxAnalyzer analyzer, Package newPackage, List<LispSexp> list) {
    for (int i = 1; i < list.size(); i++) {
      LispSexp sexp = list.get(i);
      String packageName = getStringDesignator(sexp, analyzer.annotations, analyzer.symbolManager);
      if (packageName == null) {
        analyzer.annotations.highlightError(sexp, "package name (string designator) expected");
      } else {
        Package aPackage = analyzer.symbolManager.getPackage(packageName);
        if (aPackage == null) {
          analyzer.annotations.highlightError(sexp, "unknown package");
        } else {
          newPackage.addUse(packageName);
        }
      }
    }
  }
}
