package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalyzerContext;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LexicalSymbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

import static org.ax1.lisp.analysis.symbol.LexicalSymbol.newLexicalVariable;

public class AnalyzeLetStar implements FormAnalyzer {

  @Override
  public void analyze(AnalyzerContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      form.setErrorMessage("LET* needs at least 1 argument");
      return;
    }
    LispList list1 = list.get(1).getList();
    if (list1 == null) {
      list.get(1).setErrorMessage("Variable binding list expected");
      return;
    }
    List<LexicalSymbol> variables = new ArrayList<>();
    List<LispSexp> varList = list1.getSexpList();
    for (LispSexp var : varList) {
      var.addLexicalVariables(variables);
      LexicalSymbol newLexicalVariable = analyzeLetStarVar(context, var);
      if (newLexicalVariable != null) variables.add(newLexicalVariable);
    }
    SyntaxAnalyzer.INSTANCE.analyzeForms(context, form.getSexpList(), 2);
  }

  private LexicalSymbol analyzeLetStarVar(AnalyzerContext context, @NotNull LispSexp sexp) {
    LispSymbol symbol = sexp.getSymbol();
    LispList varWithInit = sexp.getList();
    if (symbol == null && varWithInit == null) {
      sexp.setErrorMessage("Variable binding expected");
      return null;
    }
    if (symbol == null) {
      List<LispSexp> varWithInitList = varWithInit.getSexpList();
      if (varWithInitList.isEmpty() || varWithInitList.size() > 2) {
        varWithInit.setErrorMessage("Variable binding expected");
        return null;
      }
      sexp = varWithInitList.get(0);
      if (sexp.getSymbol() == null) {
        sexp.setErrorMessage("Expected variable name");
        return null;
      }
      if (varWithInitList.size() == 2) {
        LispSexp init = varWithInitList.get(1);
        SyntaxAnalyzer.INSTANCE.analyzeForm(context, init);
      }
    }
    return newLexicalVariable(sexp.getSymbolName());
  }
}
