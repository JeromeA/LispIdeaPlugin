package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.BaseLispElement;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

public class AnalyzeHandlerBind implements FormAnalyzer {

  @Override
  public void analyze(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      form.setErrorMessage("HANDLER-BIND needs at least 1 argument");
      return;
    }
    LispSexp bindingSexp = list.get(1);
    LispList bindingList = bindingSexp.getList();
    if (bindingList == null) {
      bindingSexp.setErrorMessage("Binding list expected");
      return;
    }
    bindingList.getSexpList().forEach(this::analyzeBinding);
    SyntaxAnalyzer.INSTANCE.analyzeForms(list, 2);
  }

  private void analyzeBinding(LispSexp binding) {
    LispList list = binding.getList();
    if (list == null || list.getSexpList().size() != 2) {
      binding.setErrorMessage("Binding (type handler) expected");
      return;
    }
    List<LispSexp> pair = list.getSexpList();
    pair.get(0).setType(BaseLispElement.Type.DATA);
    SyntaxAnalyzer.INSTANCE.analyzeForm(pair.get(1));
  }
}
