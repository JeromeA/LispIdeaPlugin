package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LexicalSymbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

public class AnalyzeWithInputFromString implements FormAnalyzer {

  @Override
  public void analyze(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      form.setErrorMessage("WITH-INPUT-FROM-STRING needs at least 1 argument");
      return;
    }
    LispList varList = list.get(1).getList();
    if (varList == null || varList.getSexpList().size() < 2 || varList.getSexpList().get(0).getSymbol() == null) {
      list.get(1).setErrorMessage("(var string) expected");
      return;
    }
    SyntaxAnalyzer.INSTANCE.analyzeForms(varList.getSexpList(), 1);
    LexicalSymbol variable = new LexicalSymbol(varList.getSexpList().get(0).getSymbolName());
    SyntaxAnalyzer.INSTANCE.analyzeFormsWithVariables(list, 2, List.of(variable));
  }

}
