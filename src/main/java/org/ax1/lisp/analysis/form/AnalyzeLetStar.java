package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class AnalyzeLetStar implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      context.highlighter.highlightError(form, "LET* needs at least 1 argument");
      return;
    }
    LispList list1 = list.get(1).getList();
    if (list1 == null) {
      context.highlighter.highlightError(list.get(1), "Variable binding list expected");
      return;
    }
    analyzeLetStarVarList(context, form, list1.getSexpList(), 0);
  }

  private void analyzeLetStarVarList(
      AnalysisContext context, LispList form, @NotNull List<LispSexp> varList, int startAt) {
    if (startAt >= varList.size()) {
      context.analyzer.analyzeForms(form.getSexpList(), 2);
    } else {
      LispSexp sexp = varList.get(startAt);
      LispSymbol symbol = sexp.getSymbol();
      LispList varWithInit = sexp.getList();
      if (symbol != null) {
        analyzeLetStarVarList(context, form, varList, startAt + 1);
      } else if (varWithInit != null) {
        List<LispSexp> varWithInitList = varWithInit.getSexpList();
        if (varWithInitList.size() != 2) {
          context.highlighter.highlightError(varWithInit, "Variable binding expected");
          return;
        }
        LispSymbol variable = varWithInitList.get(0).getSymbol();
        LispSexp init = varWithInitList.get(1);
        if (variable == null) {
          context.highlighter.highlightError(varWithInitList.get(0), "Expected variable name");
          return;
        }
        context.analyzer.analyzeForm(init);
        context.lexicalBindings.defineLexicalVariables(List.of(context.packageManager.getLocatedSymbol(variable)));
        analyzeLetStarVarList(context, form, varList, startAt + 1);
        context.lexicalBindings.dropLexicalVariables();
      } else {
        context.highlighter.highlightError(sexp, "Variable binding expected");
      }
    }
  }
}
