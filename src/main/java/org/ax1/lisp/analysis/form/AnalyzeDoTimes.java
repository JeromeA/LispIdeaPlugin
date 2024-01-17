package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalyzerContext;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LexicalSymbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;
import java.util.Set;

import static org.ax1.lisp.analysis.symbol.LexicalSymbol.newLexicalVariable;

public class AnalyzeDoTimes implements FormAnalyzer {

  @Override
  public void analyze(AnalyzerContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      form.setErrorMessage("DOTIMES needs at least 1 argument");
      return;
    }
    LispList varList = list.get(1).getList();
    if (varList == null || varList.getSexpList().size() < 2 || varList.getSexpList().size() > 3) {
      list.get(1).setErrorMessage("(var count [result]) expected");
      return;
    }
    LispSexp varName = varList.getSexpList().get(0);
    if (varName.getSymbol() == null) {
      varList.getSexpList().get(0).setErrorMessage("variable name expected");
      return;
    }
    LexicalSymbol variable = newLexicalVariable(varName.getSymbolName());
    SyntaxAnalyzer.INSTANCE.analyzeForms(context, varList.getSexpList(), 1);
    SyntaxAnalyzer.INSTANCE.analyzeFormsWithVariables(context, list, 2, Set.of(variable));
  }
}
