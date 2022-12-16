package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.LexicalVariableHelper;
import org.ax1.lisp.analysis.symbol.SymbolBinding;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;

public class AnalyzeWithInputFromString implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      context.highlighter.highlightError(form, "WITH-INPUT-FROM-STRING needs at least 1 argument");
      return;
    }
    LispList varList = list.get(1).getList();
    if (varList == null || varList.getSexpList().size() < 2 || varList.getSexpList().get(0).getSymbol() == null) {
      context.highlighter.highlightError(list.get(1), "(var string) expected");
      return;
    }
    context.analyzer.analyzeForms(varList.getSexpList(), 1);
    LispSexp varName = varList.getSexpList().get(0);
    SymbolBinding binding = LexicalVariableHelper.newLexicalVariable("WITH-INPUT-FROM-STRING",
        context.packageManager.getLocatedSymbol(varName), null);
    context.lexicalBindings.defineLexicalVariables(List.of(binding));
    context.analyzer.analyzeForms(list, 2);
    context.lexicalBindings.dropLexicalVariables();
  }

}
