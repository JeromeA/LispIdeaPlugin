package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.LocatedSymbol;
import org.ax1.lisp.analysis.symbol.SymbolBinding;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;

import java.util.*;

import static org.ax1.lisp.analysis.LexicalVariableHelper.newLexicalVariable;

public class AnalyzeLet implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      context.highlighter.highlightError(form, "LET needs at least 1 argument");
      return;
    }
    LispList list1 = list.get(1).getList();
    if (list1 == null) {
      context.highlighter.highlightError(list.get(1), "Variable binding list expected");
      return;
    }
    List<LispSexp> varList = list1.getSexpList();
    context.analyzer.analyzeForms(getInitForms(varList), 0);
    context.lexicalBindings.defineLexicalVariables(getLetVariables(context, varList));
    context.analyzer.analyzeForms(list, 2);
    context.lexicalBindings.dropLexicalVariables();
  }

  private Collection<LispSexp> getInitForms(List<LispSexp> varList) {
    List<LispSexp> result = new ArrayList<>();
    for (LispSexp sexp : varList) {
      LispList list = sexp.getList();
      if (list != null) {
        List<LispSexp> sexpList = list.getSexpList();
        if (sexpList.size() == 2) {
          result.add(sexpList.get(1));
        }
      }
    }
    return result;
  }

  private List<SymbolBinding> getLetVariables(AnalysisContext context, @NotNull List<LispSexp> varList) {
    List<SymbolBinding> result = new ArrayList<>();
    for (LispSexp sexp : varList) {
      LispList list = sexp.getList();
      if (sexp.getSymbol() != null) {
        LocatedSymbol locatedSymbol = context.packageManager.getLocatedSymbol(sexp);
        result.add(newLexicalVariable("LET", locatedSymbol, "nil"));
      } else if (list != null) {
        List<LispSexp> sexpList = list.getSexpList();
        if (sexpList.size() < 1 || sexpList.get(0).getSymbol() == null) {
          context.highlighter.highlightError(list, "Expected var init form");
          continue;
        }
        LocatedSymbol locatedSymbol = context.packageManager.getLocatedSymbol(sexpList.get(0));
        String initialValue = sexpList.size() < 2 ? "nil" : sexpList.get(1).getText();
        result.add(newLexicalVariable("LET", locatedSymbol, initialValue));
      } else {
        context.highlighter.highlightError(sexp, "Expected var binding");
      }
    }
    return result;
  }

}
