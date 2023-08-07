package org.ax1.lisp.analysis.form;

import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

import static org.ax1.lisp.analysis.form.LambdaAnalyzer.analyzeLambda;

public class AnalyzeLambda implements FormAnalyzer {

  @Override
  public void analyze(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      form.setErrorMessage("LAMBDA needs at least 1 argument");
      return;
    }
    analyzeLambda("LAMBDA", form, 1);
  }
}
