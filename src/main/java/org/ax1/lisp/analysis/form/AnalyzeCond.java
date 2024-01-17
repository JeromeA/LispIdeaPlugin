package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalyzerContext;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;
import java.util.stream.Collectors;

import static org.ax1.lisp.analysis.BaseLispElement.Type.CODE;

public class AnalyzeCond implements FormAnalyzer {

  @Override
  public void analyze(AnalyzerContext context, LispList form) {
    List<LispSexp> sexpList = form.getSexpList();
    for (LispSexp sexp : sexpList.stream().skip(1).collect(Collectors.toList())) {
      LispList condCase = sexp.getList();
      if (condCase == null || condCase.getSexpList().isEmpty()) {
        sexp.setErrorMessage("(test form*) expected");
        continue;
      }
      condCase.setType(CODE);
      SyntaxAnalyzer.INSTANCE.analyzeForms(context, condCase.getSexpList(), 0);
    }
  }
}
