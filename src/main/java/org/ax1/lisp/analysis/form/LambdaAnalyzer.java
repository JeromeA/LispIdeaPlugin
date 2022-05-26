package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.LexicalBindingManager;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
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

import static org.ax1.lisp.analysis.symbol.Symbol.clSymbol;

public class LambdaAnalyzer {

  private static final Set<Symbol> KEYWORDS =
      Set.of(clSymbol("&BODY"), clSymbol("&KEY"), clSymbol("&OPTIONAL"), clSymbol("&REST"));

  /**
   * Analyze a lambda function, starting from the lambda list at the specified index in the form.
   */
  public static void analyzeLambda(SyntaxAnalyzer analyzer, LispList form, int lambdaListIndex) {
    List<LispSexp> list = form.getSexpList();
    LispList lambdaList = list.get(lambdaListIndex).getList();
    if (lambdaList == null) {
      analyzer.annotations.highlightError(list.get(lambdaListIndex), "Lambda list expected");
      return;
    }
    List<LispSymbol> variables = getVariables(analyzer, lambdaList);
    try(LexicalBindingManager.LexicalScope lexicalScope = analyzer.lexicalBindings.defineLexicalVariables(form, variables)) {
      analyzer.analyzeForms(list, lambdaListIndex + 1);
    }
  }

  @NotNull
  private static List<LispSymbol> getVariables(SyntaxAnalyzer analyzer, LispList lambdaList) {
    List<LispSymbol> result = new ArrayList<>();
    for (LispSexp parameterSpecifier : lambdaList.getSexpList()) {
      LispSymbol lispSymbol = parameterSpecifier.getSymbol();
      if (lispSymbol != null) {
        Symbol symbol = analyzer.packageManager.getSymbol(lispSymbol);
        if (KEYWORDS.contains(symbol)) {
          analyzer.annotations.highlightConstant(lispSymbol);
        } else {
          result.add(lispSymbol);
        }
        continue;
      }
      LispList list = parameterSpecifier.getList();
      if (list != null && !list.getSexpList().isEmpty()) {
        List<LispSexp> varInit = list.getSexpList();
        LispSymbol varName = varInit.get(0).getSymbol();
        if (varName == null) {
          analyzer.annotations.highlightError(varInit.get(0), "Variable name expected");
          continue;
        }
        result.add(varName);
        if (varInit.size() > 1) {
          // TODO: this should happen inside the lexical env of the previous variables.
          analyzer.analyzeForm(varInit.get(1));
        }
        continue;
      }
      analyzer.annotations.highlightError(parameterSpecifier, "Parameter specifier expected");
    }
    return result;
  }
}
