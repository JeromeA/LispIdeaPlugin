package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.symbol.Package;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;
import org.ax1.lisp.psi.impl.LispStringDesignator;

import java.util.List;

import static org.ax1.lisp.analysis.BaseLispElement.Type.*;
import static org.ax1.lisp.analysis.NameDesignators.getLispStringDesignator;

public class AnalyzeDefpackage implements FormAnalyzer {

  @Override
  public void analyze(LispList form) {
    List<LispSexp> formList = form.getSexpList();
    formList.get(0).setType(KEYWORD);
    if (formList.size() < 2) {
      form.setErrorMessage("DEFPACKAGE needs at least 1 argument");
      return;
    }
    LispSexp sexp1 = formList.get(1);
    LispStringDesignator packageName = getLispStringDesignator(sexp1);
    if (packageName == null) {
      sexp1.setErrorMessage("Package name (as a string designator) expected");
      return;
    }
    packageName.setType(PACKAGE_DEFINITION);
    Package definition = new Package(packageName.getValue());
    formList.stream().skip(2).forEach(sexp -> analyzeOption(sexp, definition));
    packageName.setPackageDefinition(definition);
  }

  private void analyzeOption(LispSexp optionSexp, Package definition) {
    LispList optionList = optionSexp.getList();
    if (optionList == null || optionList.getSexpList().size() < 1) {
      optionSexp.setErrorMessage("Option (as a list) expected");
      return;
    }
    List<LispSexp> list = optionList.getSexpList();
    LispSexp sexp0 = list.get(0);
    if (sexp0.getSymbol() == null || !sexp0.getText().startsWith(":")) {
      optionSexp.setErrorMessage("Option expected");
      return;
    }
    LispSymbolName optionName = sexp0.getSymbolName();
    optionName.setType(KEYWORD);
    switch(optionName.getValue()) {
      case "EXPORT":
        analyzeOptionExport(definition, list);
        break;
      case "USE":
        analyzeOptionUses(definition, list);
        break;
      case "IMPORT-FROM":
        analyzeOptionImportFrom(definition, optionList);
        break;
      case "DOCUMENTATION":
        analyzeOptionDocumentation(definition, optionList);
        break;
      case "NICKNAMES":
        analyzeOptionNicknames(definition, optionList);
        break;
      case "SHADOW":
      case "SHADOWING-IMPORT-FROM":
      case "INTERN":
      case "SIZE":
        break;
      default:
        sexp0.setErrorMessage("DEFPACKAGE option name expected");
    }
  }

  private void analyzeOptionNicknames(Package definition, LispList optionList) {
    List<LispSexp> list = optionList.getSexpList();
    if (list.size() < 2) {
      optionList.setErrorMessage(":NICKNAMES option takes at least 1 argument");
      return;
    }
    for (int i = 1; i < list.size(); i++) {
      LispSexp sexp = list.get(i);
      LispStringDesignator stringDesignator = getLispStringDesignator(sexp);
      if (stringDesignator == null) {
        sexp.setErrorMessage("Symbol name (string designator) expected");
      } else {
        definition.addNickname(stringDesignator.getValue());
      }
    }
  }

  private void analyzeOptionImportFrom(Package definition, LispList optionList) {
    List<LispSexp> list = optionList.getSexpList();
    if (list.size() < 2) {
      optionList.setErrorMessage("IMPORT-FROM option takes at least 1 argument");
      return;
    }
    LispStringDesignator packageNameDesignator = getLispStringDesignator(list.get(1));
    if (packageNameDesignator == null) {
      list.get(1).setErrorMessage("Package name (string designator) expected");
      return;
    }
    String packageName = packageNameDesignator.getValue();
    for (int i = 2; i < list.size(); i++) {
      LispSexp sexp = list.get(i);
      LispStringDesignator stringDesignator = getLispStringDesignator(sexp);
      if (stringDesignator == null) {
        sexp.setErrorMessage("Symbol name (string designator) expected");
      } else {
        definition.addImportFrom(stringDesignator.getValue(), packageName);
      }
    }
  }

  private void analyzeOptionDocumentation(Package definition, LispList optionList) {
    if (optionList.getSexpList().size() != 2) {
      optionList.setErrorMessage("DOCUMENTATION option takes 1 argument");
      return;
    }
    LispSexp descriptionSexp = optionList.getSexpList().get(1);
    if (descriptionSexp.getString() == null) {
      descriptionSexp.setErrorMessage("Description string expected");
      return;
    }
    definition.description = descriptionSexp.getString().getStringContent().getValue();
  }

  private void analyzeOptionUses(Package definition, List<LispSexp> list) {
    for (int i = 1; i < list.size(); i++) {
      LispSexp sexp = list.get(i);
      LispStringDesignator stringDesignator = sexp.getStringDesignator();
      if (stringDesignator == null) {
        sexp.setErrorMessage("package name (string designator) expected");
      } else {
        stringDesignator.setType(PACKAGE_USAGE);
        definition.addUse(stringDesignator.getValue());
      }
    }
  }

  private void analyzeOptionExport(Package definition, List<LispSexp> list) {
    for (int i = 1; i < list.size(); i++) {
      LispSexp sexp = list.get(i);
      LispStringDesignator stringDesignator = getLispStringDesignator(sexp);
      if (stringDesignator == null) {
        sexp.setErrorMessage("Symbol name (string designator) expected");
      } else {
        definition.addExport(stringDesignator.getValue());
      }
    }
  }
}
