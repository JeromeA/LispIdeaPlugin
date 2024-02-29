package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalyzerContext;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.*;

import java.util.List;

import static org.ax1.lisp.analysis.BaseLispElement.Type.*;

public class AnalyzeSlotValue implements FormAnalyzer {

  @Override
  public void analyze(AnalyzerContext context, LispList form) {
    List<LispSexp> sexpList = form.getSexpList();
    if (sexpList.size() != 3) {
      form.setErrorMessage("SLOT-VALUE needs exactly 2 arguments: object, slot-name");
      return;
    }
    SyntaxAnalyzer.INSTANCE.analyzeForms(context, sexpList, 1);
    LispQuoted quoted = sexpList.get(2).getQuoted();
    if (quoted != null) {
      LispSymbolName symbol = quoted.getSexp().getSymbolName();
      if (symbol != null) {
        symbol.setType(SLOT_USAGE, context.packageContext);
      } else {
        quoted.getSexp().setErrorMessage("Slot name expected");
      }
    }
  }

}
