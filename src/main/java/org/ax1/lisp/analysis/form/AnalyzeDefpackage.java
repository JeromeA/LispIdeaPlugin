package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LispPackage;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;

import static org.ax1.lisp.analysis.StringDesignator.getStringDesignator;

public class AnalyzeDefpackage implements FormAnalyzer {

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.annotations.highlightKeyword(form);
    List<LispSexp> formList = form.getSexpList();
    if (formList.size() < 2) {
      analyzer.annotations.highlightError(form, "DEFPACKAGE needs at least 1 argument");
      return;
    }
    LispSexp sexp1 = formList.get(1);
    String packageName = getStringDesignator(sexp1, analyzer.annotations, analyzer.packageManager);
    if (packageName == null) {
      analyzer.annotations.highlightError(sexp1, "Package name (as a string designator) expected");
      return;
    }
    PackageDefinition definition = new PackageDefinition(packageName);
    formList.stream().skip(2).forEach(sexp -> analyzeOption(analyzer, sexp, definition));
    if (sexp1.getSymbol() != null) {
      definition.setDefinition(sexp1.getSymbol());
    }
    analyzer.scannedPackages.add(definition);
  }

  private void analyzeOption(SyntaxAnalyzer analyzer, LispSexp sexp, PackageDefinition definition) {
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
        analyzeOptionUses(analyzer, definition, list);
        break;
      default:
        analyzer.annotations.highlightError(optionSymbol, "option name expected");
    }
  }

  private void analyzeOptionUses(SyntaxAnalyzer analyzer, PackageDefinition definition, List<LispSexp> list) {
    for (int i = 1; i < list.size(); i++) {
      LispSexp sexp = list.get(i);
      String usedPackageName = getStringDesignator(sexp, analyzer.annotations, analyzer.packageManager);
      if (usedPackageName == null) {
        analyzer.annotations.highlightError(sexp, "package name (string designator) expected");
      } else {
        LispPackage usedPackage = analyzer.packageManager.getPackage(usedPackageName);
        if (usedPackage == null) {
          analyzer.annotations.highlightError(sexp, "unknown package");
        } else {
          definition.addUse(usedPackageName);
          LispSymbol symbol = sexp.getSymbol();
          if (symbol != null) {
            usedPackage.getDefinition().addUsage(symbol);
          }
        }
      }
    }
  }
}
