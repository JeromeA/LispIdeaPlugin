package org.ax1.lisp.analysis;

import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class AnalyzeLetStar implements Analyzer {

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      analyzer.highlightError(form, "LET* needs at least 1 argument");
      return;
    }
    LispList list1 = list.get(1).getList();
    if (list1 == null) {
      analyzer.highlightError(list.get(1), "Variable binding list expected");
      return;
    }
    analyzeLetStarVarList(analyzer, form, list1.getSexpList(), 0);
  }

  private void analyzeLetStarVarList(
      SyntaxAnalyzer analyzer, LispList form, @NotNull List<LispSexp> varList, int startAt) {
    if (startAt >= varList.size()) {
      analyzer.analyzeForms(form.getSexpList(), 2);
    } else {
      LispSexp sexp = varList.get(startAt);
      LispSymbol symbol = sexp.getSymbol();
      LispList varWithInit = sexp.getList();
      if (symbol != null) {
        analyzeLetStarVarList(analyzer, form, varList, startAt + 1);
      } else if (varWithInit != null) {
        List<LispSexp> varWithInitList = varWithInit.getSexpList();
        if (varWithInitList.size() != 2) {
          analyzer.highlightError(varWithInit, "Variable binding expected");
          return;
        }
        LispSymbol variable = varWithInitList.get(0).getSymbol();
        LispSexp init = varWithInitList.get(1);
        if (variable == null) {
          analyzer.highlightError(varWithInitList.get(0), "Expected variable name");
          return;
        }
        analyzer.analyzeForm(init);
        analyzer.lexicalBindings.defineLexicalVariables(form, List.of(variable));
        analyzeLetStarVarList(analyzer, form, varList, startAt + 1);
        analyzer.lexicalBindings.dropLexicalVariables();
      } else {
        analyzer.highlightError(sexp, "Variable binding expected");
      }
    }
  }
}
