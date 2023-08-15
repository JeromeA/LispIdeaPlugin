package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LexicalVariable;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class AnalyzeDo implements FormAnalyzer {

  @Override
  public void analyze(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      form.setErrorMessage("DO needs at least 2 arguments");
      return;
    }
    LispList list1 = list.get(1).getList();
    if (list1 == null) {
      list.get(1).setErrorMessage("Variable binding list expected");
      return;
    }
    List<LispSexp> varList = list1.getSexpList();
    List<LexicalVariable> variables = getVariables(varList);
    SyntaxAnalyzer.INSTANCE.analyzeForms(getInitForms(varList), 0);
    SyntaxAnalyzer.INSTANCE.analyzeFormsWithVariables(getStepForms(varList), 0, variables);
    LispList list2 = list.get(2).getList();
    if (list2 == null || list2.getSexpList().isEmpty()) {
      list.get(2).setErrorMessage("Test/result form expected");
      return;
    }
    List<LispSexp> testResultList = list2.getSexpList();
    SyntaxAnalyzer.INSTANCE.analyzeFormsWithVariables(testResultList, 0, variables);
    SyntaxAnalyzer.INSTANCE.analyzeFormsWithVariables(list, 3, variables);
  }

  private Collection<LispSexp> getInitForms(List<LispSexp> varList) {
    List<LispSexp> result = new ArrayList<>();
    for (LispSexp sexp : varList) {
      LispList list = sexp.getList();
      if (list != null) {
        List<LispSexp> sexpList = list.getSexpList();
        if (sexpList.size() >= 2) {
          result.add(sexpList.get(1));
        }
      }
    }
    return result;
  }

  private Collection<LispSexp> getStepForms(List<LispSexp> varList) {
    List<LispSexp> result = new ArrayList<>();
    for (LispSexp sexp : varList) {
      LispList list = sexp.getList();
      if (list != null) {
        List<LispSexp> sexpList = list.getSexpList();
        if (sexpList.size() >= 3) {
          result.add(sexpList.get(2));
        }
        if (sexpList.size() >= 4) {
          sexpList.get(3).setErrorMessage("Unexpected form");
        }
      }
    }
    return result;
  }

  private List<LexicalVariable> getVariables(@NotNull List<LispSexp> varList) {
    List<LexicalVariable> result = new ArrayList<>();
    for (LispSexp sexp : varList) {
      LispList list = sexp.getList();
      if (sexp.isSymbol()) {
        result.add(new LexicalVariable(sexp.getSymbolName()));
      } else if (list != null) {
        List<LispSexp> sexpList = list.getSexpList();
        if (sexpList.size() < 1 || sexpList.get(0).getSymbol() == null) {
          list.setErrorMessage("Expected var init form");
          continue;
        }
        result.add(new LexicalVariable(sexpList.get(0).getSymbolName()));
      } else {
        sexp.setErrorMessage("Expected var binding");
      }
    }
    return result;
  }
}
