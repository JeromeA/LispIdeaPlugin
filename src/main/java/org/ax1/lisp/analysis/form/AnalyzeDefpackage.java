package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

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
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    List<LispSexp> formList = form.getSexpList();
    if (formList.size() < 2) {
      context.highlighter.highlightError(form, "DEFPACKAGE needs at least 1 argument");
      return;
    }
    LispSexp sexp1 = formList.get(1);
    String packageName = getStringDesignator(context, sexp1);
    if (packageName == null) {
      context.highlighter.highlightError(sexp1, "Package name (as a string designator) expected");
      return;
    }
    PackageDefinition definition = new PackageDefinition(packageName);
    definition.setDefinition(sexp1);
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
    LispSexp optionName = list.get(0);
    if (optionName.getSymbol() == null) {
      context.highlighter.highlightError(optionName, "Option name expected");
      return;
    }
    context.highlighter.highlightKeyword(optionName);
    Symbol optionSymbol = context.getSymbol(optionName);
    switch(optionSymbol.getName()) {
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
      case "SHADOW":
      case "SHADOWING-IMPORT-FROM":
      case "INTERN":
      case "SIZE":
        break;
      default:
        context.highlighter.highlightError(optionName, "DEFPACKAGE option name expected");
    }
  }

  private void analyzeOptionImportFrom(AnalysisContext context, PackageDefinition definition, LispList optionList) {
    List<LispSexp> list = optionList.getSexpList();
    if (list.size() < 2) {
      context.highlighter.highlightError(optionList, "IMPORT-FROM option takes at least 1 argument");
      return;
    }
    getStringDesignator(context, list.get(1));
    // TODO: parse and use the rest of the form.
  }

  private void analyzeOptionDocumentation(AnalysisContext context, PackageDefinition definition, LispList optionList) {
    if (optionList.getSexpList().size() != 2) {
      context.highlighter.highlightError(optionList, "DOCUMENTATION option takes 1 argument");
      return;
    }
    LispSexp descriptionSexp = optionList.getSexpList().get(1);
    if (descriptionSexp.getNode().getFirstChildNode().getElementType() != STRING) {
      context.highlighter.highlightError(descriptionSexp, "Description string expected");
      return;
    }
    definition.setDescription(getString(descriptionSexp));
  }

  private void analyzeOptionUses(AnalysisContext context, PackageDefinition definition, List<LispSexp> list) {
    for (int i = 1; i < list.size(); i++) {
      LispSexp sexp = list.get(i);
      String stringDesignator = getStringDesignator(context, sexp);
      if (stringDesignator == null) {
        context.highlighter.highlightError(sexp, "package name (string designator) expected");
      } else {
        definition.use.put(stringDesignator, sexp);
      }
    }
  }

  private void analyzeOptionExport(AnalysisContext context, PackageDefinition definition, List<LispSexp> list) {
    for (int i = 1; i < list.size(); i++) {
      LispSexp sexp = list.get(i);
      String stringDesignator = getStringDesignator(context, sexp);
      if (stringDesignator == null) {
        context.highlighter.highlightError(sexp, "Symbol name (string designator) expected");
      } else {
        definition.exports.put(stringDesignator, sexp);
      }
    }
  }
}
