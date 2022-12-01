package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.LexicalVariableHelper;
import org.ax1.lisp.analysis.SymbolBinding;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;

public class AnalyzeWithOutputToString implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      context.highlighter.highlightError(form, "WITH-OUTPUT-TO-STRING needs at least 1 argument");
      return;
    }
    LispList varList = list.get(1).getList();
    if (varList == null) {
      context.highlighter.highlightError(list.get(1), "(var) expected");
      return;
    }
    if (varList.getSexpList().size() < 1 || varList.getSexpList().get(0).getSymbol() == null) {
      context.highlighter.highlightError(list.get(1), "(var) expected");
      return;
    }
    LispSymbol var = varList.getSexpList().get(0).getSymbol();
    SymbolBinding binding = LexicalVariableHelper.newLexicalVariable("WITH-OUTPUT-TO-STRING",
        context.packageManager.getLocatedSymbol(var), null);
    context.lexicalBindings.defineLexicalVariables(List.of(binding));
    context.analyzer.analyzeForms(list, 2);
    context.lexicalBindings.dropLexicalVariables();
  }

}
