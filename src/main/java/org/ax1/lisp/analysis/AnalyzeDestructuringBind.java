package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.Package;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class AnalyzeDestructuringBind implements Analyzer {

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      analyzer.highlightError(form, "DESTRUCTURING-BIND needs at least 2 arguments.");
      return;
    }
    LispSexp sexp1 = list.get(1);
    LispList list1 = sexp1.getList();
    if (list1 == null) {
      analyzer.highlightError(sexp1, "Destructuring lambda list expected");
      return;
    }
    analyzer.lexicalBindings.defineLexicalVariables(form, getDestructuringBindVariableSymbols(analyzer, list1.getSexpList()));
    analyzer.analyzeForms(list, 2);
    analyzer.lexicalBindings.dropLexicalVariables();
  }

  private List<LispSymbol> getDestructuringBindVariableSymbols(SyntaxAnalyzer analyzer, @NotNull List<LispSexp> lambdaList) {
    Package cl = analyzer.symbolManager.getPackage("CL");
    Set<Symbol> keywords = Set.of(
        cl.intern("&ALLOW-OTHER-KEYS"),
        cl.intern("&REST"),
        cl.intern("&KEY"));
    List<LispSymbol> result = new ArrayList<>();
    for (LispSexp sexp : lambdaList) {
      LispSymbol symbolName = sexp.getSymbol();
      LispList list = sexp.getList();
      if (symbolName != null) {
        Symbol symbol = analyzer.symbolManager.getSymbol(symbolName.getText());
        if (keywords.contains(symbol)) {
          analyzer.highlightConstant(symbolName);
        } else {
          result.add(symbolName);
        }
      } else if (list != null) {
        result.addAll(getDestructuringBindVariableSymbols(analyzer, list.getSexpList()));
      } else {
        analyzer.highlightError(sexp, "Destructuring lambda list expected");
      }
    }
    return result;
  }
}
