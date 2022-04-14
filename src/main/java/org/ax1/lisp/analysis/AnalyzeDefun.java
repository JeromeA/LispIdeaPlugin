package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.SymbolStack.LexicalDrop;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.ax1.lisp.parsing.LispSyntaxHighlighter.FUNCTION_DECLARATION;

public class AnalyzeDefun implements Analyzer {

  private final Type type;

  public AnalyzeDefun(Type type) {
    this.type = type;
  }

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      analyzer.highlightError(form, type.name() + " needs at least 2 arguments.");
      return;
    }
    LispSexp sexp1 = list.get(1);
    LispSymbol symbol1 = sexp1.getSymbol();
    if (symbol1 != null) {
      analyzer.functions.registerSpecialDefinition(form, symbol1);
      analyzer.highlight(symbol1, FUNCTION_DECLARATION);
    } else {
      // TODO: check DEFUN SETF case.
    }
    LispList lambdaList = list.get(2).getList();
    if (lambdaList == null) {
      analyzer.highlightError(list.get(2), "Lambda list expected");
      return;
    }
    List<LispSymbol> variables = lambdaList.getSexpList().stream()
        .map(LispSexp::getSymbol)
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
    try(LexicalDrop lexicalDrop = analyzer.variables.registerLexicalDefinitions(form, variables)) {
      analyzer.analyzeForms(list, 3);
    }
  }

  public static enum Type {
    DEFUN,
    DEFMACRO
  }
}
