package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.LexicalVariableHelper;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

public class AnalyzeHandlerCase implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      context.highlighter.highlightError(form, "HANDLER-CASE needs at least 1 argument");
      return;
    }
    context.analyzer.analyzeForm(list.get(1));
    for (int i = 2; i < list.size(); i++) {
      LispList clause = list.get(i).getList();
      if (clause == null || clause.getSexpList().size() < 2) {
        context.highlighter.highlightError(list.get(i), "(typespec ([var]) declaration* form*) error clause expected");
        continue;
      }
      context.highlighter.highlightConstant(clause.getSexpList().get(0));
      LispSexp sexp1 = clause.getSexpList().get(1);
      if (sexp1.getList() == null || sexp1.getList().getSexpList().size() > 1) {
        context.highlighter.highlightError(sexp1, "([var]) variable declaration expected");
        continue;
      }
      if (sexp1.getList().getSexpList().isEmpty()) {
        context.analyzer.analyzeForms(clause.getSexpList(), 2);
      } else {
        LispSexp varName = sexp1.getList().getSexpList().get(0);
        SymbolDefinition binding = LexicalVariableHelper.newLexicalVariable("HANDLER-CASE error clause",
            context.getLocatedSymbol(varName.getSymbol()), null);
        context.lexicalBindings.defineLexicalVariables(List.of(binding));
        context.analyzer.analyzeForms(clause.getSexpList(), 2);
        context.lexicalBindings.dropLexicalVariables();
      }
    }
  }
}
