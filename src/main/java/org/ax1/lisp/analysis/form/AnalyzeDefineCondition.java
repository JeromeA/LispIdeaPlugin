package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalyzerContext;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

import static org.ax1.lisp.analysis.BaseLispElement.Type.*;

public class AnalyzeDefineCondition implements FormAnalyzer {

  @Override
  public void analyze(AnalyzerContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 4) {
      form.setErrorMessage("DEFINE-CONDITION needs at least 3 arguments");
      return;
    }

    if (!list.get(1).isSymbol()) {
      list.get(1).setErrorMessage("Expecting name of the condition");
      list.stream().skip(2).forEach(s -> s.setType(ERROR));
      return;
    }
    list.get(1).getSymbolName().setType(CONDITION_DEFINITION, context.packageContext);

    LispList parentList = list.get(2).getList();
    if (parentList == null) {
      list.get(2).setErrorMessage("Expecting parent list");
      list.stream().skip(3).forEach(s -> s.setType(ERROR));
      return;
    }
    parentList.setType(CODE);
    parentList.getSexpList().forEach(parentCondition -> analyzeParent(context, parentCondition));

    LispList slotList = list.get(3).getList();
    if (slotList == null) {
      list.get(3).setErrorMessage("Expecting slot list");
      return;
    }
    slotList.setType(CODE);
    slotList.getSexpList().forEach(this::analyzeSlot);
  }

  private void analyzeSlot(LispSexp slot) {
    if (slot.isSymbol()) {
      slot.getSymbolName().setType(CODE);
      return;
    }
    if (slot.getList() != null && !slot.getList().getSexpList().isEmpty() && slot.getList().getSexpList().get(0).isSymbol()) {
      slot.getList().setType(CODE);
      slot.getList().getSexpList().forEach(s -> s.setType(CODE));
    }
  }

  private void analyzeParent(AnalyzerContext context, LispSexp parentCondition) {
    if (!parentCondition.isSymbol()) {
      parentCondition.setErrorMessage("Expecting parent condition");
      return;
    }
    parentCondition.getSymbolName().setType(CONDITION_USAGE, context.packageContext);
  }
}
