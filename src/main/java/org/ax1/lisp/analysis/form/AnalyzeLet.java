package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalyzerContext;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LexicalSymbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.jetbrains.annotations.NotNull;

import java.util.*;

import static org.ax1.lisp.analysis.symbol.LexicalSymbol.newLexicalVariable;

public class AnalyzeLet implements FormAnalyzer {

  @Override
  public void analyze(AnalyzerContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      form.setErrorMessage("LET needs at least 1 argument");
      return;
    }
    LispList list1 = list.get(1).getList();
    if (list1 == null) {
      list.get(1).setErrorMessage("Variable binding list expected");
      return;
    }
    List<LispSexp> varList = list1.getSexpList();
    SyntaxAnalyzer.INSTANCE.analyzeForms(context, getInitForms(varList), 0);
    SyntaxAnalyzer.INSTANCE.analyzeFormsWithVariables(context, list, 2, getLetVariables(varList));
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

  private List<LexicalSymbol> getLetVariables(@NotNull List<LispSexp> varList) {
    List<LexicalSymbol> result = new ArrayList<>();
    for (LispSexp sexp : varList) {
      LispList list = sexp.getList();
      if (sexp.isSymbol()) {
        result.add(newLexicalVariable(sexp.getSymbolName()));
      } else if (list != null) {
        List<LispSexp> sexpList = list.getSexpList();
        if (sexpList.size() < 1 || sexpList.get(0).getSymbol() == null) {
          list.setErrorMessage("Expected var init form");
          continue;
        }
        result.add(newLexicalVariable(sexpList.get(0).getSymbolName()));
      } else {
        sexp.setErrorMessage("Expected var binding");
      }
    }
    return result;
  }
}
