package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.LexicalBindingManager;
import org.ax1.lisp.analysis.LexicalVariableHelper;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

import static com.google.common.collect.ImmutableList.toImmutableList;

public class AnalyzeMultipleValueBind implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      context.highlighter.highlightError(form, "MULTIPLE-VALUE-BIND needs at least 2 arguments.");
      return;
    }
    LispSexp sexp1 = list.get(1);
    LispList list1 = sexp1.getList();
    if (list1 == null) {
      context.highlighter.highlightError(sexp1, "Variable list expected");
      return;
    }
    List<SymbolDefinition> variables = getVariableSymbols(context, list1.getSexpList()).stream()
        .map(context::getLocatedSymbol)
        .map(locatedSymbol -> LexicalVariableHelper.newLexicalVariable("MULTIPLE-VALUE-BIND", locatedSymbol, null))
        .collect(toImmutableList());
    try (LexicalBindingManager.LexicalScope ignored = context.lexicalBindings.defineLexicalVariables(variables)) {
      context.analyzer.analyzeForms(list, 2);
    }
  }

  private List<LispSymbol> getVariableSymbols(AnalysisContext context, @NotNull List<LispSexp> lambdaList) {
    List<LispSymbol> result = new ArrayList<>();
    for (LispSexp sexp : lambdaList) {
      if (sexp.isSymbol()) {
        result.add(sexp.getSymbol());
      } else {
        context.highlighter.highlightError(sexp, "Variable name expected");
      }
    }
    return result;
  }
}
