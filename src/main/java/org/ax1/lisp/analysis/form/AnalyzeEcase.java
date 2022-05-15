package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

public class AnalyzeEcase implements FormAnalyzer {

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.annotations.highlightKeyword(form);
    List<LispSexp> sexpList = form.getSexpList();
    if (sexpList.size() < 2) {
      analyzer.annotations.highlightError(form, "ECASE needs at least 1 argument");
      return;
    }
    analyzer.analyzeForm(sexpList.get(1));
    sexpList.stream().skip(2).forEach(sexp -> {
      LispList list = sexp.getList();
      if (list == null || list.getSexpList().size() < 2) {
        analyzer.annotations.highlightError(sexp, "key-form clause expected");
      } else {
        analyzer.annotations.highlightConstant(list.getSexpList().get(0));
        analyzer.analyzeForms(list.getSexpList(), 1);
      }
    });
  }
}
