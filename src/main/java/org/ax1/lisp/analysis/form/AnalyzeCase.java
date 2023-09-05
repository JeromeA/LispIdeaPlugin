package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

import static org.ax1.lisp.analysis.BaseLispElement.Type.DATA;
import static org.ax1.lisp.analysis.SyntaxAnalyzer.ANALYZE_QUOTE;

public class AnalyzeCase implements FormAnalyzer {

  private final Type type;

  public AnalyzeCase(Type type) {
    this.type = type;
  }

  @Override
  public void analyze(LispList form) {
    List<LispSexp> sexpList = form.getSexpList();
    if (sexpList.size() < 2) {
      form.setErrorMessage(type + " needs at least 1 argument");
      return;
    }
    SyntaxAnalyzer.INSTANCE.analyzeForm(sexpList.get(1));
    sexpList.stream().skip(2).forEach(sexp -> {
      LispList list = sexp.getList();
      if (list == null || list.getSexpList().isEmpty()) {
        sexp.setErrorMessage("key-form clause expected");
        return;
      }
      LispSexp keyListDesignator = list.getSexpList().get(0);
      if (keyListDesignator.getList() != null) {
        keyListDesignator.getList().getSexpList().forEach(ANALYZE_QUOTE::analyze);
      } else {
        keyListDesignator.setType(DATA);
      }
      SyntaxAnalyzer.INSTANCE.analyzeForms(list.getSexpList(), 1);
    });
  }

  public enum Type {
    CASE, ECASE, CCASE,
    TYPECASE, CTYPECASE, ETYPECASE
  }
}
