package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.LexicalBindingManager.LexicalDrop;
import org.ax1.lisp.analysis.symbol.LispPackage;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.FUNCTION_DECLARATION;

public class AnalyzeDefun implements Analyzer {

  private final Type type;

  public AnalyzeDefun(Type type) {
    this.type = type;
  }

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.annotations.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      analyzer.annotations.highlightError(form, type.name() + " needs at least 2 arguments.");
      return;
    }
    LispSexp sexp1 = list.get(1);
    LispSymbol symbol1 = sexp1.getSymbol();
    if (symbol1 != null) {
      analyzer.symbolManager.getFunction(symbol1.getText()).setDefinition(form, symbol1);
      analyzer.annotations.highlight(symbol1, FUNCTION_DECLARATION);
    } else {
      // TODO: check DEFUN SETF case.
    }
    LispList lambdaList = list.get(2).getList();
    if (lambdaList == null) {
      analyzer.annotations.highlightError(list.get(2), "Lambda list expected");
      return;
    }
    List<LispSymbol> variables = getVariables(analyzer, lambdaList);
    try(LexicalDrop lexicalDrop = analyzer.lexicalBindings.defineLexicalVariables(form, variables)) {
      analyzer.analyzeForms(list, 3);
    }
  }

  @NotNull
  private List<LispSymbol> getVariables(SyntaxAnalyzer analyzer, LispList lambdaList) {
    LispPackage cl = analyzer.symbolManager.getPackage("CL");
    Set<Symbol> keywords = Set.of(
        cl.intern(analyzer.symbolManager, "&BODY"),
        cl.intern(analyzer.symbolManager, "&REST"),
        cl.intern(analyzer.symbolManager, "&KEY"));
    List<LispSymbol> result = new ArrayList<>();
    for (LispSexp lispSexp : lambdaList.getSexpList()) {
      LispSymbol lispSymbol = lispSexp.getSymbol();
      if (lispSymbol != null) {
        Symbol symbol = analyzer.symbolManager.getSymbol(lispSymbol.getText());
        if (keywords.contains(symbol)) {
          analyzer.annotations.highlightConstant(lispSymbol);
        } else {
          result.add(lispSymbol);
        }
      }
    }
    return result;
  }

  public enum Type {
    DEFUN,
    DEFMACRO
  }
}
