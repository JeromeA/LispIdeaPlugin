package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalyzerContext;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;
import java.util.Set;

import static org.ax1.lisp.analysis.BaseLispElement.Type.KEYWORD;

public class AnalyzeEvalWhen implements FormAnalyzer {

  private static final Set<String> SITUATIONS =
      Set.of("COMPILE-TOPLEVEL", "LOAD-TOPLEVEL", "EXECUTE", "COMPILE", "LOAD", "EVAL");

  @Override
  public void analyze(AnalyzerContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      form.setErrorMessage("EVAL-WHEN needs at least 1 argument");
      return;
    }
    LispList situationList = list.get(1).getList();
    if (situationList == null) {
      list.get(1).setErrorMessage("List of situations expected");
      return;
    }
    for (LispSexp situation : situationList.getSexpList()) {
      if (situation.isSymbol() && SITUATIONS.contains(situation.getSymbolName().getLispName())) {
        situation.setType(KEYWORD);
      } else {
        situation.setErrorMessage("Invalid situation");
      }
    }

    SyntaxAnalyzer.INSTANCE.analyzeForms(context, list, 2);
  }
}
