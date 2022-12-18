package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.*;
import org.ax1.lisp.analysis.LexicalBindingManager.LexicalScope;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static com.google.common.collect.ImmutableList.toImmutableList;
import static org.ax1.lisp.analysis.symbol.Symbol.clSymbol;

public class LambdaAnalyzer {

  private static final Set<Symbol> KEYWORDS =
      Set.of(clSymbol("&BODY"), clSymbol("&KEY"), clSymbol("&OPTIONAL"), clSymbol("&REST"));

  /**
   * Analyze a lambda function, starting from the lambda list at the specified index in the form.
   */
  public static void analyzeLambda(String formName, AnalysisContext context, LispList form, int lambdaListIndex) {
    List<LispSexp> list = form.getSexpList();
    LispList lambdaList = list.get(lambdaListIndex).getList();
    if (lambdaList == null) {
      context.highlighter.highlightError(list.get(lambdaListIndex), "Lambda list expected");
      return;
    }
    List<SymbolDefinition> variables = getVariables(context, lambdaList).stream()
        .map(context::getLocatedSymbol)
        .map(locatedSymbol -> LexicalVariableHelper.newLexicalVariable(formName, locatedSymbol, null))
        .collect(toImmutableList());
    try(LexicalScope ignored = context.lexicalBindings.defineLexicalVariables(variables)) {
      context.analyzer.analyzeForms(list, lambdaListIndex + 1);
    }
  }

  @NotNull
  private static List<LispSexp> getVariables(AnalysisContext context, LispList lambdaList) {
    List<LispSexp> result = new ArrayList<>();
    for (LispSexp parameterSpecifier : lambdaList.getSexpList()) {
      if (parameterSpecifier.isSymbol()) {
        Symbol symbol = context.getSymbol(parameterSpecifier);
        if (KEYWORDS.contains(symbol)) {
          context.highlighter.highlightConstant(parameterSpecifier);
        } else {
          result.add(parameterSpecifier);
        }
        continue;
      }
      LispList list = parameterSpecifier.getList();
      if (list != null && !list.getSexpList().isEmpty()) {
        List<LispSexp> varInit = list.getSexpList();
        LispSexp varName = varInit.get(0);
        if (varName.getSymbol() == null) {
          context.highlighter.highlightError(varName, "Variable name expected");
          continue;
        }
        result.add(varName);
        if (varInit.size() > 1) {
          // TODO: this should happen inside the lexical env of the previous variables.
          context.analyzer.analyzeForm(varInit.get(1));
        }
        continue;
      }
      context.highlighter.highlightError(parameterSpecifier, "Parameter specifier expected");
    }
    return result;
  }
}
