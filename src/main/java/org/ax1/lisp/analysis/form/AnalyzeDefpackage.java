package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LispPackage;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;

import static org.ax1.lisp.analysis.Strings.getString;
import static org.ax1.lisp.analysis.Strings.getStringDesignator;
import static org.ax1.lisp.psi.LispTypes.STRING;

/**
 * This analyzer can be used from three different context:
 * - From {@code PackageAnalyzer}, to populate analyzer.scannedPackages with the definition of all packages.
 * - From {@link SyntaxAnalyzer}, to reference all symbols (definitions and usages).
 * - From {@code LispAnnotator}, to annotate the code with highlighting and syntax errors.
 */
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
    String packageName = getStringDesignator(analyzer, sexp1);
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

  private void analyzeOption(SyntaxAnalyzer analyzer, LispSexp optionSexp, PackageDefinition definition) {
    LispList optionList = optionSexp.getList();
    if (optionList == null || optionList.getSexpList().size() < 1) {
      analyzer.annotations.highlightError(optionSexp, "Option (as a list) expected");
      return;
    }
    List<LispSexp> list = optionList.getSexpList();
    LispSymbol optionName = list.get(0).getSymbol();
    if (optionName == null) {
      analyzer.annotations.highlightError(optionList.getSexpList().get(0), "Option name expected");
      return;
    }
    analyzer.annotations.highlightKeyword(optionName);
    Symbol optionSymbol = analyzer.packageManager.getSymbol(optionName);
    switch(optionSymbol.getName()) {
      case "EXPORT":
        analyzeOptionExport(analyzer, definition, list);
        break;
      case "USE":
        analyzeOptionUses(analyzer, definition, list);
        break;
      case "IMPORT-FROM":
        analyzeOptionImportFrom(analyzer, definition, optionList);
        break;
      case "DOCUMENTATION":
        analyzeOptionDocumentation(analyzer, definition, optionList);
        break;
      case "NICKNAME":
      case "SHADOW":
      case "SHADOWING-IMPORT-FROM":
      case "INTERN":
      case "SIZE":
        break;
      default:
        analyzer.annotations.highlightError(optionName, "DEFPACKAGE option name expected");
    }
  }

  private void analyzeOptionImportFrom(SyntaxAnalyzer analyzer, PackageDefinition definition, LispList optionList) {
    List<LispSexp> list = optionList.getSexpList();
    if (list.size() < 2) {
      analyzer.annotations.highlightError(optionList, "IMPORT-FROM option takes at least 1 argument");
      return;
    }
    getStringDesignator(analyzer, list.get(1));
    // TODO: parse and use the rest of the form.
  }

  private void analyzeOptionDocumentation(SyntaxAnalyzer analyzer, PackageDefinition definition, LispList optionList) {
    if (optionList.getSexpList().size() != 2) {
      analyzer.annotations.highlightError(optionList, "DOCUMENTATION option takes 1 argument");
      return;
    }
    LispSexp descriptionSexp = optionList.getSexpList().get(1);
    if (descriptionSexp.getNode().getFirstChildNode().getElementType() != STRING) {
      analyzer.annotations.highlightError(descriptionSexp, "Description string expected");
      return;
    }
    definition.setDescription(getString(descriptionSexp));
  }

  private void analyzeOptionUses(SyntaxAnalyzer analyzer, PackageDefinition definition, List<LispSexp> list) {
    for (int i = 1; i < list.size(); i++) {
      LispSexp sexp = list.get(i);
      String usedPackageName = getStringDesignator(analyzer, sexp);
      if (usedPackageName == null) {
        analyzer.annotations.highlightError(sexp, "package name (string designator) expected");
      } else {
        LispPackage usedPackage = analyzer.packageManager.getPackage(usedPackageName);
        definition.addUse(usedPackageName);
        if (usedPackage == null) {
          analyzer.annotations.highlightError(sexp, "Unknown package");
        } else {
          LispSymbol symbol = sexp.getSymbol();
          if (symbol != null) {
            usedPackage.getDefinition().addUsage(symbol);
          }
        }
      }
    }
  }

  private void analyzeOptionExport(SyntaxAnalyzer analyzer, PackageDefinition definition, List<LispSexp> list) {
    for (int i = 1; i < list.size(); i++) {
      LispSexp sexp = list.get(i);
      LispSymbol symbol = sexp.getSymbol();
      String exportedSymbolName = getStringDesignator(analyzer, sexp);
      if (exportedSymbolName == null) {
        analyzer.annotations.highlightError(sexp, "Symbol name (string designator) expected");
      } else if (symbol != null) {
        definition.addExport(exportedSymbolName, symbol);
      }
    }
  }
}
