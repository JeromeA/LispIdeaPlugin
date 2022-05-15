package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;

public class AnalyzeDolist implements FormAnalyzer {

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.annotations.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      analyzer.annotations.highlightError(form, "DOLIST needs at least 1 argument");
      return;
    }
    LispList varList = list.get(1).getList();
    if (varList == null || varList.getSexpList().size() < 2 || varList.getSexpList().size() > 3) {
      analyzer.annotations.highlightError(list.get(1), "(var list [result]) expected");
      return;
    }
    LispSymbol var = varList.getSexpList().get(0).getSymbol();
    if (var == null) {
      analyzer.annotations.highlightError(varList.getSexpList().get(0), "variable name expected");
      return;
    }
    analyzer.analyzeForm(varList.getSexpList().get(1));
    analyzer.lexicalBindings.defineLexicalVariables(form, List.of(var));
    analyzer.analyzeForms(list, 2);
    analyzer.lexicalBindings.dropLexicalVariables();
  }
}
