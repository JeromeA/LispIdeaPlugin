package org.ax1.lisp.loop;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.LexicalVariableHelper;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;
import java.util.stream.Collectors;

import static com.google.common.collect.ImmutableList.toImmutableList;

public class LoopParserBase {
  private AnalysisContext context;
  private List<LispSexp> sexpList;
  private int index = 0;
  private int lexicalDepth = 0;

  public void init(AnalysisContext context, LispList form) {
    this.context = context;
    this.sexpList = form.getSexpList();
  }

  public void cleanup() {
    for (int i = 0; i < lexicalDepth; i++) {
      context.lexicalBindings.dropLexicalVariables();
    }
  }

  public void skip() {
    index++;
  }

  public void keyword() {
    context.highlighter.highlightKeyword(sexpList.get(index));
    index++;
  }

  public void analyzeForm() {
    context.analyzer.analyzeForm(sexpList.get(index));
    index++;
  }

  public void declareVariable() {
    List<SymbolDefinition> locatedVariables = getVariables(sexpList.get(index)).stream()
        .map(context::getLocatedSymbol)
        .map(locatedSymbol -> LexicalVariableHelper.newLexicalVariable("LOOP", locatedSymbol, null))
        .collect(toImmutableList());
    context.lexicalBindings.defineLexicalVariables(locatedVariables);
    lexicalDepth++;
    index++;
  }

  private List<LispSymbol> getVariables(LispSexp sexp) {
    if (sexp.getSymbol() != null) {
      if (sexp.getText().equals("nil")) {
        context.highlighter.highlightKeyword(sexp);
        return List.of();
      }
      return List.of(sexp.getSymbol());
    }
    LispList list = sexp.getList();
    if (list != null) {
      List<LispSexp> sexpList = list.getSexpList();
      return sexpList.stream().flatMap(s -> getVariables(s).stream()).collect(Collectors.toList());
    }
    context.highlighter.highlightError(sexp, "Variable name expected");
    return List.of();
  }

  public void error(String text) {
    context.highlighter.highlightError(sexpList.get(index), text);
  }

  public void missingExpression() {
    context.highlighter.highlightError(sexpList.get(index - 1), "Missing expression");
  }

  public void missingVariable() {
    context.highlighter.highlightError(sexpList.get(index - 1), "Missing variable name");
  }
}
