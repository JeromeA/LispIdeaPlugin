package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

public class AnalyzeHandlerBind implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      context.highlighter.highlightError(form, "HANDLER-BIND needs at least 1 argument");
      return;
    }
    LispSexp bindingSexp = list.get(1);
    LispList bindingList = bindingSexp.getList();
    if (bindingList == null) {
      context.highlighter.highlightError(bindingSexp, "Binding list expected");
      return;
    }
    bindingList.getSexpList().forEach(binding -> analyzeBinding(context, binding));
    context.analyzer.analyzeForms(list, 2);
  }

  private void analyzeBinding(AnalysisContext context, LispSexp binding) {
    LispList list = binding.getList();
    if (list == null || list.getSexpList().size() != 2) {
      context.highlighter.highlightError(binding, "Binding (type handler) expected");
      return;
    }
    List<LispSexp> pair = list.getSexpList();
    context.highlighter.highlightConstant(pair.get(0));
    context.analyzer.analyzeForm(pair.get(1));
  }
}
