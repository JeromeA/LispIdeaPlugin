package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.LexicalVariableHelper;
import org.ax1.lisp.analysis.SymbolBinding;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;

public class AnalyzeDoTimes implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      context.highlighter.highlightError(form, "DOTIMES needs at least 1 argument");
      return;
    }
    LispList varList = list.get(1).getList();
    if (varList == null || varList.getSexpList().size() < 2 || varList.getSexpList().size() > 3) {
      context.highlighter.highlightError(list.get(1), "(var count [result]) expected");
      return;
    }
    LispSymbol var = varList.getSexpList().get(0).getSymbol();
    if (var == null) {
      context.highlighter.highlightError(varList.getSexpList().get(0), "variable name expected");
      return;
    }
    context.analyzer.analyzeForms(varList.getSexpList(), 1);
    SymbolBinding binding = LexicalVariableHelper.newLexicalVariable("DOTIMES",
        context.packageManager.getLocatedSymbol(var), null);
    context.lexicalBindings.defineLexicalVariables(List.of(binding));
    context.analyzer.analyzeForms(list, 2);
    context.lexicalBindings.dropLexicalVariables();
  }
}
