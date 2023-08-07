package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.BaseLispElement;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

import static org.ax1.lisp.analysis.BaseLispElement.Type.CODE;

public class AnalyzeCase implements FormAnalyzer {

  @Override
  public void analyze(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      form.setErrorMessage("CASE needs at least 1 argument");
      return;
    }
    SyntaxAnalyzer.INSTANCE.analyzeForm(list.get(1));
    for (int i = 2; i < list.size(); i++) {
      LispList clause = list.get(i).getList();
      if (clause == null || clause.getSexpList().isEmpty()) {
        list.get(i).setErrorMessage("normal clause expected");
        continue;
      }
      clause.setType(CODE);
      clause.getSexpList().get(0).setType(BaseLispElement.Type.DATA);
      SyntaxAnalyzer.INSTANCE.analyzeForms(clause.getSexpList(), 1);
    }
  }
}
