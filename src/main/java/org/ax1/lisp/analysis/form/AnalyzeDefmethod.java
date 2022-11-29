package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.LexicalBindingManager;
import org.ax1.lisp.analysis.LexicalBindingManager.LexicalScope;
import org.ax1.lisp.analysis.LocatedSymbol;
import org.ax1.lisp.analysis.symbol.Symbol;
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
import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.FUNCTION_DECLARATION;

public class AnalyzeDefmethod implements FormAnalyzer {

  private static final Set<Symbol> KEYWORDS =
      Stream.of("&KEY", "&OPTIONAL", "&REST")
          .map(Symbol::clSymbol)
          .collect(Collectors.toSet());

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      context.highlighter.highlightError(form, "DEFMETHOD needs at least 2 arguments.");
      return;
    }
    LispSexp sexp1 = list.get(1);
    LispSymbol symbol1 = sexp1.getSymbol();
    if (symbol1 == null) {
      context.highlighter.highlightError(sexp1, "Function name expected");
      return;
    }
    context.addMethodDefinition(symbol1);
    context.highlighter.highlight(symbol1, FUNCTION_DECLARATION);

    int arg = 2;
    // Skip method qualifiers.
    while (arg < list.size() && list.get(arg).getList() == null) arg++;

    if (arg == list.size()) {
      context.highlighter.highlightError(form, "Missing lambda list");
      return;
    }
    LispList lambdaList = list.get(arg).getList();
    if (lambdaList == null) {
      context.highlighter.highlightError(list.get(arg), "Lambda list expected");
      return;
    }

    List<LocatedSymbol> variables = getVariables(context, lambdaList).stream()
        .map(context.packageManager::getLocatedSymbol)
        .collect(toImmutableList());
    try(LexicalScope ignored = context.lexicalBindings.defineLexicalVariables(variables)) {
      context.analyzer.analyzeForms(list, arg + 1);
    }
  }

  @NotNull
  private static List<LispSymbol> getVariables(AnalysisContext context, LispList lambdaList) {
    List<LispSymbol> result = new ArrayList<>();
    for (LispSexp lispSexp : lambdaList.getSexpList()) {
      LispSymbol variable = getVariable(context, lispSexp);
      if (variable != null) result.add(variable);
    }
    return result;
  }

  private static LispSymbol getVariable(AnalysisContext context, LispSexp sexp) {
    LispSymbol parsedSymbol = sexp.getSymbol();
    LispList list = sexp.getList();
    if (list != null) {
      List<LispSexp> specialized = list.getSexpList();
      if (specialized.size() != 2 || specialized.get(0).getSymbol() == null) {
        context.highlighter.highlightError(list, "var-specializer expected");
        return null;
      }
      parsedSymbol = specialized.get(0).getSymbol();
    }
    if (parsedSymbol != null) {
      Symbol symbol = context.packageManager.getSymbol(parsedSymbol);
      if (KEYWORDS.contains(symbol)) {
        context.highlighter.highlightConstant(parsedSymbol);
      } else {
        return parsedSymbol;
      }
    }
    return null;
  }
}
