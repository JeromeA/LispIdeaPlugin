package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.LexicalBindingManager.LexicalScope;
import org.ax1.lisp.analysis.LexicalVariableHelper;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.ax1.lisp.psi.LispSymbolName;
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
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      context.highlighter.highlightError(form, "DEFMETHOD needs at least 2 arguments.");
      return;
    }
    LispSexp functionName = list.get(1);
    if (functionName.getSymbol() == null) {
      context.highlighter.highlightError(functionName, "Function name expected");
      return;
    }
    context.addMethodDefinition(functionName.getSymbol(), "");
    context.highlighter.highlight(functionName.getSymbolName(), FUNCTION_DECLARATION);

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

    List<SymbolDefinition> variables = getVariables(context, lambdaList).stream()
        .map(context::getLocatedSymbol)
        .map(locatedSymbol -> LexicalVariableHelper.newLexicalVariable("DEFMETHOD", locatedSymbol, null))
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
    LispList list = sexp.getList();
    if (list != null) {
      List<LispSexp> specialized = list.getSexpList();
      if (specialized.size() != 2 || specialized.get(0).getSymbol() == null) {
        context.highlighter.highlightError(list, "var-specializer expected");
        return null;
      }
      sexp = specialized.get(0);
    }
    if (sexp.isSymbol()) {
      Symbol symbol = context.getSymbol(sexp.getSymbol());
      if (KEYWORDS.contains(symbol)) {
        context.highlighter.highlightConstant(sexp);
      } else {
        return sexp.getSymbol();
      }
    }
    return null;
  }
}
