package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.LexicalVariableHelper;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

public class AnalyzeWithOpenFile implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      context.highlighter.highlightError(form, "WITH-OPEN-FILE needs at least 1 argument");
      return;
    }
    LispList varList = list.get(1).getList();
    if (varList == null || varList.getSexpList().size() < 2 || varList.getSexpList().get(0).getSymbol() == null) {
      context.highlighter.highlightError(list.get(1), "Stream declaration expected");
      return;
    }
    context.analyzer.analyzeForms(varList.getSexpList(), 1);
    LispSexp varName = varList.getSexpList().get(0);
    SymbolDefinition symbolDefinition = LexicalVariableHelper.newLexicalVariable("WITH-OPEN-FILE",
        context.getLocatedSymbol(varName.getSymbol()), null);
    context.lexicalBindings.defineLexicalVariables(List.of(symbolDefinition));
    context.analyzer.analyzeForms(list, 2);
    context.lexicalBindings.dropLexicalVariables();
  }

}
