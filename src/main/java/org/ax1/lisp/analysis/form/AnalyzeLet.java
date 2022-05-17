package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class AnalyzeLet implements FormAnalyzer {

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.annotations.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      analyzer.annotations.highlightError(form, "LET needs at least 1 argument");
      return;
    }
    LispList list1 = list.get(1).getList();
    if (list1 == null) {
      analyzer.annotations.highlightError(list.get(1), "Variable binding list expected");
      return;
    }
    List<LispSexp> varList = list1.getSexpList();
    analyzer.analyzeForms(getInitForms(varList), 0);
    analyzer.lexicalBindings.defineLexicalVariables(form, getLetVariableSymbols(analyzer, varList));
    analyzer.analyzeForms(list, 2);
    analyzer.lexicalBindings.dropLexicalVariables();
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

  private List<LispSymbol> getLetVariableSymbols(SyntaxAnalyzer analyzer, @NotNull List<LispSexp> varList) {
    List<LispSymbol> result = new ArrayList<>();
    for (LispSexp sexp : varList) {
      LispSymbol symbol = sexp.getSymbol();
      LispList list = sexp.getList();
      if (symbol != null) {
        result.add(symbol);
      } else if (list != null) {
        List<LispSexp> sexpList = list.getSexpList();
        if (sexpList.size() < 1 || sexpList.get(0).getSymbol() == null) {
          analyzer.annotations.highlightError(list, "Expected var init form");
          continue;
        }
        result.add(sexpList.get(0).getSymbol());
      } else {
        analyzer.annotations.highlightError(sexp, "Expected var binding");
      }
    }
    return result;
  }

}