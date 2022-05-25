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

import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.FUNCTION_DECLARATION;

public class AnalyzeDefmethod implements FormAnalyzer {

  private static final Set<Symbol> KEYWORDS =
      Stream.of("&KEY", "&OPTIONAL", "&REST")
          .map(Symbol::clSymbol)
          .collect(Collectors.toSet());

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.annotations.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      analyzer.annotations.highlightError(form, "DEFMETHOD needs at least 2 arguments.");
      return;
    }
    LispSexp sexp1 = list.get(1);
    LispSymbol symbol1 = sexp1.getSymbol();
    if (symbol1 == null) {
      analyzer.annotations.highlightError(sexp1, "Function name expected");
      return;
    }
    analyzer.packageManager.getFunction(symbol1.getText()).addMethod(symbol1);
    analyzer.annotations.highlight(symbol1, FUNCTION_DECLARATION);

    int arg = 2;
    // Skip method qualifiers.
    while (arg < list.size() && list.get(arg).getList() == null) arg++;

    if (arg == list.size()) {
      analyzer.annotations.highlightError(form, "Missing lambda list");
      return;
    }
    LispList lambdaList = list.get(arg).getList();
    if (lambdaList == null) {
      analyzer.annotations.highlightError(list.get(arg), "Lambda list expected");
      return;
    }

    List<LispSymbol> variables = getVariables(analyzer, lambdaList);
    try(LexicalBindingManager.LexicalScope lexicalScope = analyzer.lexicalBindings.defineLexicalVariables(form, variables)) {
      analyzer.analyzeForms(list, arg + 1);
    }
  }

  @NotNull
  private static List<LispSymbol> getVariables(SyntaxAnalyzer analyzer, LispList lambdaList) {
    List<LispSymbol> result = new ArrayList<>();
    for (LispSexp lispSexp : lambdaList.getSexpList()) {
      LispSymbol variable = getVariable(analyzer, lispSexp);
      if (variable != null) result.add(variable);
    }
    return result;
  }

  private static LispSymbol getVariable(SyntaxAnalyzer analyzer, LispSexp sexp) {
    LispSymbol parsedSymbol = sexp.getSymbol();
    LispList list = sexp.getList();
    if (list != null) {
      List<LispSexp> specialized = list.getSexpList();
      if (specialized.size() != 2 || specialized.get(0).getSymbol() == null) {
        analyzer.annotations.highlightError(list, "var-specializer expected");
        return null;
      }
      parsedSymbol = specialized.get(0).getSymbol();
    }
    if (parsedSymbol != null) {
      Symbol symbol = analyzer.packageManager.getSymbol(parsedSymbol);
      if (KEYWORDS.contains(symbol)) {
        analyzer.annotations.highlightConstant(parsedSymbol);
      } else {
        return parsedSymbol;
      }
    }
    return null;
  }
}
