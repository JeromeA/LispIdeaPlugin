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
    String packageName = getStringDesignator(formList.get(1), analyzer.annotations, analyzer.packageManager);
    if (packageName == null) {
      analyzer.annotations.highlightError(formList.get(1), "Package name (as a string designator) expected");
      return;
    }
    PackageDefinition definition = new PackageDefinition(packageName);
    formList.stream().skip(2).forEach(sexp -> analyzeOption(analyzer, sexp, definition));
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
      String packageName = getStringDesignator(sexp, analyzer.annotations, analyzer.packageManager);
      if (packageName == null) {
        analyzer.annotations.highlightError(sexp, "package name (string designator) expected");
      } else {
        LispPackage aPackage = analyzer.packageManager.getPackage(packageName);
        if (aPackage == null) {
          analyzer.annotations.highlightError(sexp, "unknown package");
        } else {
          definition.addUse(packageName);
        }
      }
    }
  }
}