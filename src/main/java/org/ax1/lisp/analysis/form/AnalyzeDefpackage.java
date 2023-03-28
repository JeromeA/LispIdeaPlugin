package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;
import org.ax1.lisp.psi.impl.LispStringDesignator;

import java.util.List;

import static org.ax1.lisp.analysis.NameDesignators.getLispStringDesignator;

/**
 * This analyzer can be used from three different context:
 * - From {@code PackageAnalyzer}, to populate analyzer.scannedPackages with the definition of all packages.
 * - From {@link SyntaxAnalyzer}, to reference all symbols (definitions and usages).
 * - From {@code LispAnnotator}, to annotate the code with highlighting and syntax errors.
 */
public class AnalyzeDefpackage implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    List<LispSexp> formList = form.getSexpList();
    if (formList.size() < 2) {
      context.highlighter.highlightError(form, "DEFPACKAGE needs at least 1 argument");
      return;
    }
    LispSexp sexp1 = formList.get(1);
    LispStringDesignator packageName = getLispStringDesignator(context, sexp1);
    if (packageName == null) {
      context.highlighter.highlightError(sexp1, "Package name (as a string designator) expected");
      return;
    }
    PackageDefinition definition = new PackageDefinition(packageName.getValue());
    definition.setDefinition(packageName);
    formList.stream().skip(2).forEach(sexp -> analyzeOption(context, sexp, definition));
    context.result.addDefPackage(definition);
  }

  private void analyzeOption(AnalysisContext context, LispSexp optionSexp, PackageDefinition definition) {
    LispList optionList = optionSexp.getList();
    if (optionList == null || optionList.getSexpList().size() < 1) {
      context.highlighter.highlightError(optionSexp, "Option (as a list) expected");
      return;
    }
    List<LispSexp> list = optionList.getSexpList();
    LispSexp sexp0 = list.get(0);
    if (sexp0.getSymbol() == null || !sexp0.getText().startsWith(":")) {
      context.highlighter.highlightError(sexp0, "Option name expected");
      return;
    }
    Symbol symbol = context.getSymbol(sexp0.getSymbol());
    context.highlighter.highlightKeyword(sexp0);
    switch(symbol.getName()) {
      case "EXPORT":
        analyzeOptionExport(context, definition, list);
        break;
      case "USE":
        analyzeOptionUses(context, definition, list);
        break;
      case "IMPORT-FROM":
        analyzeOptionImportFrom(context, definition, optionList);
        break;
      case "DOCUMENTATION":
        analyzeOptionDocumentation(context, definition, optionList);
        break;
      case "NICKNAMES":
        analyzeOptionNicknames(context, definition, optionList);
        break;
      case "SHADOW":
      case "SHADOWING-IMPORT-FROM":
      case "INTERN":
      case "SIZE":
        break;
      default:
        context.highlighter.highlightError(sexp0, "DEFPACKAGE option name expected");
    }
  }

  private void analyzeOptionNicknames(AnalysisContext context, PackageDefinition definition, LispList optionList) {
    List<LispSexp> list = optionList.getSexpList();
    if (list.size() < 2) {
      context.highlighter.highlightError(optionList, ":NICKNAMES option takes at least 1 argument");
      return;
    }
    for (int i = 1; i < list.size(); i++) {
      LispSexp sexp = list.get(i);
      LispStringDesignator stringDesignator = getLispStringDesignator(context, sexp);
      if (stringDesignator == null) {
        context.highlighter.highlightError(sexp, "Symbol name (string designator) expected");
      } else {
        definition.addNickname(stringDesignator.getValue());
      }
    }
  }

  private void analyzeOptionImportFrom(AnalysisContext context, PackageDefinition definition, LispList optionList) {
    List<LispSexp> list = optionList.getSexpList();
    if (list.size() < 2) {
      context.highlighter.highlightError(optionList, "IMPORT-FROM option takes at least 1 argument");
      return;
    }
    LispStringDesignator packageNameDesignator = getLispStringDesignator(context, list.get(1));
    if (packageNameDesignator == null) {
      context.highlighter.highlightError(list.get(1), "Package name (string designator) expected");
      return;
    }
    String packageName = packageNameDesignator.getValue();
    for (int i = 2; i < list.size(); i++) {
      LispSexp sexp = list.get(i);
      LispStringDesignator stringDesignator = getLispStringDesignator(context, sexp);
      if (stringDesignator == null) {
        context.highlighter.highlightError(sexp, "Symbol name (string designator) expected");
      } else {
        definition.addImportFrom(stringDesignator.getValue(), packageName);
      }
    }
  }

  private void analyzeOptionDocumentation(AnalysisContext context, PackageDefinition definition, LispList optionList) {
    if (optionList.getSexpList().size() != 2) {
      context.highlighter.highlightError(optionList, "DOCUMENTATION option takes 1 argument");
      return;
    }
    LispSexp descriptionSexp = optionList.getSexpList().get(1);
    if (descriptionSexp.getString() == null) {
      context.highlighter.highlightError(descriptionSexp, "Description string expected");
      return;
    }
    definition.setDescription(descriptionSexp.getString().getStringContent().getValue());
  }

  private void analyzeOptionUses(AnalysisContext context, PackageDefinition definition, List<LispSexp> list) {
    for (int i = 1; i < list.size(); i++) {
      LispSexp sexp = list.get(i);
      LispStringDesignator stringDesignator = sexp.getStringDesignator();
      if (stringDesignator == null) {
        context.highlighter.highlightError(sexp, "package name (string designator) expected");
      } else {
        definition.use.put(stringDesignator.getValue(), stringDesignator);
      }
    }
  }

  private void analyzeOptionExport(AnalysisContext context, PackageDefinition definition, List<LispSexp> list) {
    for (int i = 1; i < list.size(); i++) {
      LispSexp sexp = list.get(i);
      LispStringDesignator stringDesignator = getLispStringDesignator(context, sexp);
      if (stringDesignator == null) {
        context.highlighter.highlightError(sexp, "Symbol name (string designator) expected");
      } else {
        definition.exports.put(stringDesignator.getValue(), stringDesignator);
      }
    }
  }
}
