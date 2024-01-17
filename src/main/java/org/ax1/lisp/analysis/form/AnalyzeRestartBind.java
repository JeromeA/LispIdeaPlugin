package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalyzerContext;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

import static org.ax1.lisp.analysis.BaseLispElement.Type.DATA;

public class AnalyzeRestartBind implements FormAnalyzer {

  @Override
  public void analyze(AnalyzerContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      form.setErrorMessage("RESTART-BIND needs at least 1 argument");
      return;
    }
    LispSexp bindingSexp = list.get(1);
    LispList bindingList = bindingSexp.getList();
    if (bindingList == null) {
      bindingSexp.setErrorMessage("Binding list expected");
      return;
    }
    bindingList.getSexpList().forEach(sexp -> analyzeBinding(context, sexp));
    SyntaxAnalyzer.INSTANCE.analyzeForms(context, list, 2);
  }

  private void analyzeBinding(AnalyzerContext context, LispSexp sexp) {
    LispList list = sexp.getList();
    if (list == null || list.getSexpList().size() < 2) {
      sexp.setErrorMessage("Binding expected");
      return;
    }
    List<LispSexp> sexpList = list.getSexpList();
    LispSexp sexp0 = sexpList.get(0);
    if (!sexp0.isSymbol()) {
      sexp0.setErrorMessage("Named restart expected");
      return;
    }
    sexp0.getSymbolName().setType(DATA);
    LispSexp sexp1 = sexpList.get(1);
    // TODO: this must evaluate to a function, it is typically a lambda, but can also be a reference to a function,
    //   we should check that.
    SyntaxAnalyzer.INSTANCE.analyzeForm(context, sexp1);
    // TODO: analyze the options that follow, when sexplist length is greater than 2.
  }
}
