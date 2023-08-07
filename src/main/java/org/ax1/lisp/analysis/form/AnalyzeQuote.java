package org.ax1.lisp.analysis.form;

import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import static org.ax1.lisp.analysis.BaseLispElement.Type.DATA;

public class AnalyzeQuote {

  public void analyze(LispSexp form) {
    analyzeForm(form);
  }

  private void analyzeForm(LispSexp form) {
    LispList list = form.getList();
    if (list != null) {
      list.setType(DATA);
      list.getSexpList().forEach(this::analyzeForm);
    } else {
      form.setType(DATA);
    }
  }
}
