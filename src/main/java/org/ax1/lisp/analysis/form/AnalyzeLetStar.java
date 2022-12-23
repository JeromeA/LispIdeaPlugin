package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.LocatedSymbol;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;

import java.util.List;

import static org.ax1.lisp.analysis.LexicalVariableHelper.newLexicalVariable;

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
      String initialValue = "nil";
      if (symbol == null && varWithInit == null) {
        context.highlighter.highlightError(sexp, "Variable binding expected");
        return;
      }
      if (symbol == null) {
        List<LispSexp> varWithInitList = varWithInit.getSexpList();
        if (varWithInitList.isEmpty() || varWithInitList.size() > 2) {
          context.highlighter.highlightError(varWithInit, "Variable binding expected");
          return;
        }
        sexp = varWithInitList.get(0);
        if (sexp.getSymbol() == null) {
          context.highlighter.highlightError(sexp, "Expected variable name");
          return;
        }
        if (varWithInitList.size() == 2) {
          LispSexp init = varWithInitList.get(1);
          context.analyzer.analyzeForm(init);
          initialValue = init.getText();
        }
      }
      LocatedSymbol locatedSymbol = context.getLocatedSymbol(sexp.getSymbol());
      SymbolDefinition variable = newLexicalVariable("LET*", locatedSymbol, initialValue);
      context.lexicalBindings.defineLexicalVariables(List.of(variable));
      analyzeLetStarVarList(context, form, varList, startAt + 1);
      context.lexicalBindings.dropLexicalVariables();
    }
  }

}
