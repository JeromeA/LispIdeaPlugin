package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

import static org.ax1.lisp.analysis.BaseLispElement.Type.DATA;

public class AnalyzeEcase implements FormAnalyzer {

  @Override
  public void analyze(LispList form) {
    List<LispSexp> sexpList = form.getSexpList();
    if (sexpList.size() < 2) {
      form.setErrorMessage("ECASE needs at least 1 argument");
      return;
    }
    SyntaxAnalyzer.INSTANCE.analyzeForm(sexpList.get(1));
    sexpList.stream().skip(2).forEach(sexp -> {
      LispList list = sexp.getList();
      if (list == null || list.getSexpList().size() < 2) {
        sexp.setErrorMessage("key-form clause expected");
      } else {
        LispSexp keyListDesignator = list.getSexpList().get(0);
        keyListDesignator.setType(DATA);
        if (keyListDesignator.getList() != null) {
          keyListDesignator.getList().getSexpList().forEach(s -> s.setType(DATA));
        }
        SyntaxAnalyzer.INSTANCE.analyzeForms(list.getSexpList(), 1);
      }
    });
  }
}
