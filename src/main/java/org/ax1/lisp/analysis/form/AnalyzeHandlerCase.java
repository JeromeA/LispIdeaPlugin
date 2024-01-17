package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalyzerContext;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LexicalSymbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;
import java.util.Set;

import static org.ax1.lisp.analysis.BaseLispElement.Type.*;
import static org.ax1.lisp.analysis.symbol.LexicalSymbol.newLexicalVariable;

public class AnalyzeHandlerCase implements FormAnalyzer {

  @Override
  public void analyze(AnalyzerContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      form.setErrorMessage("HANDLER-CASE needs at least 1 argument");
      return;
    }
    SyntaxAnalyzer.INSTANCE.analyzeForm(context, list.get(1));
    for (int i = 2; i < list.size(); i++) {
      LispList clause = list.get(i).getList();
      if (clause == null || clause.getSexpList().size() < 2) {
        list.get(i).setErrorMessage("(typespec ([var]) declaration* form*) error clause expected");
        continue;
      }
      clause.getSexpList().get(0).setType(DATA);
      LispSexp sexp1 = clause.getSexpList().get(1);
      if (sexp1.getList() == null || sexp1.getList().getSexpList().size() > 1) {
        sexp1.setErrorMessage("([var]) variable declaration expected");
        continue;
      }
      if (sexp1.getList().getSexpList().isEmpty()) {
        SyntaxAnalyzer.INSTANCE.analyzeForms(context, clause.getSexpList(), 2);
      } else {
        LexicalSymbol variable = newLexicalVariable(sexp1.getList().getSexpList().get(0).getSymbolName());
        SyntaxAnalyzer.INSTANCE.analyzeFormsWithVariables(context, clause.getSexpList(), 2, Set.of(variable));
      }
    }
  }
}
