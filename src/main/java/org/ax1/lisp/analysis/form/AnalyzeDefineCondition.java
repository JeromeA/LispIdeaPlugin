package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;

public class AnalyzeDefineCondition implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 4) {
      context.highlighter.highlightError(form, "DEFINE-CONDITION needs at least 3 arguments");
      return;
    }

    LispSymbol symbol = list.get(1).getSymbol();
    if (symbol == null) {
      context.highlighter.highlightError(list.get(1), "Expecting name of the condition");
      return;
    }

    LispList parentList = list.get(2).getList();
    if (parentList == null) {
      context.highlighter.highlightError(list.get(2), "Expecting parent list");
      return;
    }

    LispList slotList = list.get(3).getList();
    if (slotList == null) {
      context.highlighter.highlightError(list.get(3), "Expecting slot list");
      return;
    }
  }
}
