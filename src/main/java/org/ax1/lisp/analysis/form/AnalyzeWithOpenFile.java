package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LexicalVariable;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;

import java.util.List;

import static org.ax1.lisp.analysis.BaseLispElement.Type.LEXICAL_VARIABLE_DEFINITION;

public class AnalyzeWithOpenFile implements FormAnalyzer {

  @Override
  public void analyze(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      form.setErrorMessage("WITH-OPEN-FILE needs at least 1 argument");
      return;
    }
    LispList varList = list.get(1).getList();
    if (varList == null || varList.getSexpList().size() < 2 || varList.getSexpList().get(0).getSymbol() == null) {
      list.get(1).setErrorMessage("Stream declaration expected");
      return;
    }
    SyntaxAnalyzer.INSTANCE.analyzeForms(varList.getSexpList(), 1);
    LexicalVariable variable = new LexicalVariable(varList.getSexpList().get(0).getSymbolName());
    SyntaxAnalyzer.INSTANCE.analyzeFormsWithVariables(list, 2, List.of(variable));
  }

}
