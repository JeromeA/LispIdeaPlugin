package org.ax1.lisp.analysis;

import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.CONSTANT;

public class AnalyzeEcase implements Analyzer {

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.highlightKeyword(form);
    List<LispSexp> sexpList = form.getSexpList();
    if (sexpList.size() < 2) {
      analyzer.highlightError(form, "ECASE needs at least 1 argument");
      return;
    }
    analyzer.analyzeForm(sexpList.get(1));
    sexpList.stream().skip(2).forEach(sexp -> {
      LispList list = sexp.getList();
      if (list == null || list.getSexpList().size() < 2) {
        analyzer.highlightError(sexp, "key-form clause expected");
      } else {
        analyzer.highlightConstant(list.getSexpList().get(0));
        analyzer.analyzeForms(list.getSexpList(), 1);
      }
    });
  }
}
