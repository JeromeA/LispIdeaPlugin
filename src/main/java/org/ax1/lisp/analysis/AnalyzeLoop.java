package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.SymbolStack.LexicalDrop;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;
import java.util.Set;

public class AnalyzeLoop implements Analyzer {

  private static final Set<String> FOR_SUBCLAUSE_KEYWORDS = Set.of("=", "across", "in", "on");

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.highlightKeyword(form.getSexpList().get(0));
    analyze(analyzer, form, 1);
  }

  private void analyze(SyntaxAnalyzer analyzer, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() <= startAt) return;
    LispSymbol symbol = list.get(startAt).getSymbol();
    if (symbol == null) {
      analyzer.highlightError(list.get(startAt), "Loop keyword expected");
      return;
    }
    switch (symbol.getText()) {
      case "for":
        analyzeFor(analyzer, form, startAt);
        break;
      case "always":
      case "append":
      case "collect":
      case "do":
      case "never":
      case "repeat":
      case "until":
      case "while":
        analyzeDo(analyzer, form, startAt);
        break;
      default:
        analyzer.highlightError(list.get(startAt), "Loop keyword expected");
    }
  }

  private void analyzeDo(SyntaxAnalyzer analyzer, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    analyzer.highlightKeyword(list.get(startAt));
    analyzer.analyzeForm(list.get(startAt + 1));
    analyze(analyzer, form, startAt + 2);
  }

  private void analyzeFor(SyntaxAnalyzer analyzer, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    analyzer.highlightKeyword(list.get(startAt));
    LispSexp sexp1 = list.get(startAt + 1);
    if (sexp1.getSymbol() == null) {
      analyzer.highlightError(sexp1, "Variable name expected");
      return;
    }
    try (LexicalDrop lexicalDrop = analyzer.variables.registerLexicalDefinitions(form, List.of(sexp1.getSymbol()))) {
      LispSexp sexp2 = list.get(startAt + 2);
      if (sexp2.getSymbol() == null || !FOR_SUBCLAUSE_KEYWORDS.contains(sexp2.getSymbol().getText())) {
        analyzer.highlightError(sexp2, "Loop keyword expected");
        return;
      }
      analyzer.highlightKeyword(sexp2);
      analyzer.analyzeForm(list.get(startAt + 3));
      analyze(analyzer, form, startAt + 4);
    }
  }
}
