package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LexicalSymbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

import static org.ax1.lisp.analysis.BaseLispElement.Type.*;

public class AnalyzeMultipleValueBind implements FormAnalyzer {

  @Override
  public void analyze(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      form.setErrorMessage("MULTIPLE-VALUE-BIND needs at least 2 arguments.");
      return;
    }
    LispSexp sexp1 = list.get(1);
    LispList list1 = sexp1.getList();
    if (list1 == null) {
      sexp1.setErrorMessage("Variable list expected");
      list.stream().skip(2).forEach(s -> s.setType(ERROR));
      return;
    }
    SyntaxAnalyzer.INSTANCE.analyzeFormsWithVariables(list, 2, getVariables(list1.getSexpList()));
  }

  private List<LexicalSymbol> getVariables(@NotNull List<LispSexp> lambdaList) {
    List<LexicalSymbol> result = new ArrayList<>();
    for (LispSexp sexp : lambdaList) {
      if (sexp.isSymbol()) {
        LispSymbolName symbolName = sexp.getSymbolName();
        result.add(new LexicalSymbol(symbolName));
      } else {
        sexp.setErrorMessage("Variable name expected");
      }
    }
    return result;
  }
}
