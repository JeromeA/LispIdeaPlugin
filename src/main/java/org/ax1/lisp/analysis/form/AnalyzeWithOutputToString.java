package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LexicalSymbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;
import java.util.Set;

public class AnalyzeWithOutputToString implements FormAnalyzer {

  @Override
  public void analyze(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      form.setErrorMessage("WITH-OUTPUT-TO-STRING needs at least 1 argument");
      return;
    }
    LispList varList = list.get(1).getList();
    if (varList == null) {
      list.get(1).setErrorMessage("(var) expected");
      return;
    }
    if (varList.getSexpList().size() < 1 || varList.getSexpList().get(0).getSymbol() == null) {
      list.get(1).setErrorMessage("(var) expected");
      return;
    }
    LexicalSymbol variable = new LexicalSymbol(varList.getSexpList().get(0).getSymbolName());
    SyntaxAnalyzer.INSTANCE.analyzeFormsWithVariables(list, 2, Set.of(variable));
  }

}
