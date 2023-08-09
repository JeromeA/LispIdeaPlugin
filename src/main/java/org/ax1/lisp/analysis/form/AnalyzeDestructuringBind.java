package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.*;
import org.ax1.lisp.analysis.symbol.LexicalVariable;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static org.ax1.lisp.analysis.BaseLispElement.Type.KEYWORD;

public class AnalyzeDestructuringBind implements FormAnalyzer {

  private static final Set<String> KEYWORDS = Set.of("&ALLOW-OTHER-KEYS", "&KEY", "&REST");

  @Override
  public void analyze(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      form.setErrorMessage("DESTRUCTURING-BIND needs at least 2 arguments.");
      return;
    }
    LispSexp sexp1 = list.get(1);
    LispList list1 = sexp1.getList();
    if (list1 == null) {
      sexp1.setErrorMessage("Destructuring lambda list expected");
      return;
    }
    SyntaxAnalyzer.INSTANCE.analyzeFormsWithVariables(list, 2, getDestructuringBindVariables(list1.getSexpList()));
  }

  private List<LexicalVariable> getDestructuringBindVariables(@NotNull List<LispSexp> lambdaList) {
    List<LexicalVariable> result = new ArrayList<>();
    for (LispSexp sexp : lambdaList) {
      LispList list = sexp.getList();
      if (sexp.isSymbol()) {
        LispSymbolName symbolName = sexp.getSymbolName();
        if (KEYWORDS.contains(symbolName.getLispName())) {
          symbolName.setType(KEYWORD);
        } else {
          result.add(new LexicalVariable(symbolName));
        }
      } else if (list != null) {
        result.addAll(getDestructuringBindVariables(list.getSexpList()));
      } else {
        sexp.setErrorMessage("Destructuring lambda list expected");
      }
    }
    return result;
  }
}
