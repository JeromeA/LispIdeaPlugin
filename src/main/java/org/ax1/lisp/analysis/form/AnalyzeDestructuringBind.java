package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.*;
import org.ax1.lisp.analysis.LexicalBindingManager.LexicalScope;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.google.common.collect.ImmutableList.toImmutableList;

public class AnalyzeDestructuringBind implements FormAnalyzer {

  private static final Set<Symbol> KEYWORDS =
      Stream.of("&ALLOW-OTHER-KEYS", "&KEY", "&REST")
          .map(Symbol::clSymbol)
          .collect(Collectors.toSet());

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      context.highlighter.highlightError(form, "DESTRUCTURING-BIND needs at least 2 arguments.");
      return;
    }
    LispSexp sexp1 = list.get(1);
    LispList list1 = sexp1.getList();
    if (list1 == null) {
      context.highlighter.highlightError(sexp1, "Destructuring lambda list expected");
      return;
    }
    List<SymbolDefinition> variables = getDestructuringBindVariableSymbols(context, list1.getSexpList()).stream()
        .map(context::getLocatedSymbol)
        .map(locatedSymbol -> LexicalVariableHelper.newLexicalVariable("DESTRUCTURING-BIND", locatedSymbol, null))
        .collect(toImmutableList());
    try (LexicalScope ignored = context.lexicalBindings.defineLexicalVariables(variables)) {
      context.analyzer.analyzeForms(list, 2);
    }
  }

  private List<LispSymbol> getDestructuringBindVariableSymbols(AnalysisContext context, @NotNull List<LispSexp> lambdaList) {
    List<LispSymbol> result = new ArrayList<>();
    for (LispSexp sexp : lambdaList) {
      LispList list = sexp.getList();
      if (sexp.isSymbol()) {
        LispSymbol fullSymbol = sexp.getSymbol();
        Symbol symbol = context.getSymbol(fullSymbol);
        if (KEYWORDS.contains(symbol)) {
          context.highlighter.highlightConstant(fullSymbol);
        } else {
          result.add(fullSymbol);
        }
      } else if (list != null) {
        result.addAll(getDestructuringBindVariableSymbols(context, list.getSexpList()));
      } else {
        context.highlighter.highlightError(sexp, "Destructuring lambda list expected");
      }
    }
    return result;
  }
}
