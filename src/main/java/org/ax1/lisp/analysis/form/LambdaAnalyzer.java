package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.LexicalBindingManager;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LispPackage;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class LambdaAnalyzer {

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
    LispPackage cl = analyzer.packageManager.getPackage("CL");
    Set<Symbol> keywords = Set.of(
        cl.intern(analyzer.packageManager, "&BODY"),
        cl.intern(analyzer.packageManager, "&OPTIONAL"),
        cl.intern(analyzer.packageManager, "&REST"),
        cl.intern(analyzer.packageManager, "&KEY"));
    List<LispSymbol> result = new ArrayList<>();
    for (LispSexp lispSexp : lambdaList.getSexpList()) {
      LispSymbol lispSymbol = lispSexp.getSymbol();
      if (lispSymbol != null) {
        Symbol symbol = analyzer.packageManager.getSymbol(lispSymbol.getText());
        if (keywords.contains(symbol)) {
          analyzer.annotations.highlightConstant(lispSymbol);
        } else {
          result.add(lispSymbol);
        }
      }
    }
    return result;
  }
}
