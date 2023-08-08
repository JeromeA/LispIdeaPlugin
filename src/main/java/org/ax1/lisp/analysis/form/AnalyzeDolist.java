package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LexicalVariable;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;
import java.util.Set;

import static org.ax1.lisp.analysis.BaseLispElement.Type.*;

public class AnalyzeDolist implements FormAnalyzer {

  @Override
  public void analyze(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      form.setErrorMessage("DOLIST needs at least 1 argument");
      return;
    }
    LispList varList = list.get(1).getList();
    if (varList == null || varList.getSexpList().size() < 2 || varList.getSexpList().size() > 3) {
      list.get(1).setErrorMessage("(var list [result]) expected");
      return;
    }
    LispSexp varName = varList.getSexpList().get(0);
    if (varName.getSymbol() == null) {
      varList.getSexpList().get(0).setErrorMessage("variable name expected");
      return;
    }
    SyntaxAnalyzer.INSTANCE.analyzeForm(varList.getSexpList().get(1));
    LexicalVariable variable = new LexicalVariable(varName.getSymbolName());
    SyntaxAnalyzer.INSTANCE.analyzeFormsWithVariables(list, 2, Set.of(variable));
  }
}
