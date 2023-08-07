package org.ax1.lisp.analysis.form;

import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

import static org.ax1.lisp.analysis.BaseLispElement.Type.FUNCTION_DEFINITION;

public class AnalyzeDefgeneric implements FormAnalyzer {

  @Override
  public void analyze(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      form.setErrorMessage("DEFGENERIC needs at least 2 arguments");
      return;
    }
    LispSexp sexp1 = list.get(1);
    if (sexp1.getSymbol() == null) {
      sexp1.setErrorMessage("Function name expected");
      return;
    }
    sexp1.getSymbolName().setType(FUNCTION_DEFINITION);
  }
}
