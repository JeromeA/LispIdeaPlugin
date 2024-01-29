package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalyzerContext;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.impl.LispStringDesignator;

import static org.ax1.lisp.analysis.BaseLispElement.Type.PACKAGE_USAGE;

public class AnalyzeInPackage implements FormAnalyzer {

  @Override
  public void analyze(AnalyzerContext context, LispList form) {
    if (form.getSexpList().size() != 2) {
      form.setErrorMessage("IN-PACKAGE needs exactly 1 argument");
      return;
    }
    LispSexp sexp1 = form.getSexpList().get(1);
    LispStringDesignator stringDesignator = sexp1.getStringDesignator();
    if (stringDesignator == null) {
      sexp1.setErrorMessage("Expected name designator");
      return;
    }

    context.packageContext = stringDesignator.getLispName();
    stringDesignator.setType(PACKAGE_USAGE);
  }
}
