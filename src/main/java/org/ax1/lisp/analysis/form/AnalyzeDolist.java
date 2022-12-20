package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.LexicalVariableHelper;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

public class AnalyzeDolist implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      context.highlighter.highlightError(form, "DOLIST needs at least 1 argument");
      return;
    }
    LispList varList = list.get(1).getList();
    if (varList == null || varList.getSexpList().size() < 2 || varList.getSexpList().size() > 3) {
      context.highlighter.highlightError(list.get(1), "(var list [result]) expected");
      return;
    }
    LispSexp varName = varList.getSexpList().get(0);
    if (varName.getSymbol() == null) {
      context.highlighter.highlightError(varList.getSexpList().get(0), "variable name expected");
      return;
    }
    context.analyzer.analyzeForm(varList.getSexpList().get(1));
    SymbolDefinition binding = LexicalVariableHelper.newLexicalVariable("DOLIST",
        context.getLocatedSymbol(varName.getSymbol()), null);
    context.lexicalBindings.defineLexicalVariables(List.of(binding));
    context.analyzer.analyzeForms(list, 2);
    context.lexicalBindings.dropLexicalVariables();
  }
}
