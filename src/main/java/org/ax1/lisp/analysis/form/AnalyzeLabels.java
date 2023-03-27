package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.LexicalBindingManager.LexicalScope;
import org.ax1.lisp.analysis.LocatedSymbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

import static org.ax1.lisp.analysis.form.LambdaAnalyzer.analyzeLambda;

public class AnalyzeLabels implements FormAnalyzer {

  private final Type type;

  public AnalyzeLabels(Type type) {
    this.type = type;
  }

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      context.highlighter.highlightError(form, type.name() + "needs at least 1 argument");
      return;
    }
    LispList list1 = list.get(1).getList();
    if (list1 == null) {
      context.highlighter.highlightError(list.get(1), "Function binding list expected");
      return;
    }
    List<LispSexp> functionList = list1.getSexpList();
    try (LexicalScope ignored = context.lexicalBindings.defineLexicalFunctions(getFunctionSymbols(context, functionList))) {
      functionList.forEach(function -> analyzeFunction(context, function));
      context.analyzer.analyzeForms(list, 2);
    }
  }

  private void analyzeFunction(AnalysisContext context, LispSexp function) {
    if (function.getList() != null && function.getList().getSexpList().size() >= 2) {
      analyzeLambda(type.name(), context, function.getList(), 1);
    }
  }

  /**
   *  Quick scan to find the name of the lexical functions. Errors are ignored, annotations will happen at a later
   *  stage, when each function is analyzed.
   */
  private List<LocatedSymbol> getFunctionSymbols(AnalysisContext context, @NotNull List<LispSexp> functionList) {
    List<LocatedSymbol> result = new ArrayList<>();
    for (LispSexp function : functionList) {
      if (function.getList() == null || function.getList().getSexpList().size() < 2) {
        context.highlighter.highlightError(function, "Function definition expected");
        continue;
      }
      LispSexp functionName = function.getList().getSexpList().get(0);
      if (functionName.getSymbol() == null) {
        context.highlighter.highlightError(function, "Function name expected");
        continue;
      }
      result.add(context.getLocatedSymbol(functionName.getSymbol()));
    }
    return result;
  }

  public enum Type {
    FLET,
    LABELS
  }
}
