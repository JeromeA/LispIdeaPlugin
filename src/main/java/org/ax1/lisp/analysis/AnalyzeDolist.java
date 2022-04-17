package org.ax1.lisp.analysis;

import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;

public class AnalyzeDolist implements Analyzer {

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      analyzer.highlightError(form, "DOLIST needs at least 1 argument");
      return;
    }
    LispList varList = list.get(1).getList();
    if (varList == null || varList.getSexpList().size() < 2 || varList.getSexpList().size() > 3) {
      analyzer.highlightError(list.get(1), "(var list [result]) expected");
      return;
    }
    LispSymbol var = varList.getSexpList().get(0).getSymbol();
    if (var == null) {
      analyzer.highlightError(varList.getSexpList().get(0), "variable name expected");
      return;
    }
    analyzer.analyzeForm(varList.getSexpList().get(1));
    analyzer.lexicalSymbols.defineLexicalVariables(form, List.of(var));
    analyzer.analyzeForms(list, 2);
    analyzer.lexicalSymbols.dropLexicalVariables();
  }
}
