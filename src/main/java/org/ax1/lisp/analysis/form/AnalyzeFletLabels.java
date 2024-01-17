package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalyzerContext;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LexicalSymbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static org.ax1.lisp.analysis.form.LambdaAnalyzer.analyzeLambda;
import static org.ax1.lisp.analysis.symbol.LexicalSymbol.newLexicalFunction;

public class AnalyzeFletLabels implements FormAnalyzer {

  private final Type type;

  public AnalyzeFletLabels(Type type) {
    this.type = type;
  }

  @Override
  public void analyze(AnalyzerContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      form.setErrorMessage(type.name() + " needs at least 1 argument");
      return;
    }
    LispList list1 = list.get(1).getList();
    if (list1 == null) {
      list.get(1).setErrorMessage("Function binding list expected");
      return;
    }
    List<LispSexp> functionList = list1.getSexpList();
    functionList.forEach(function -> analyzeFunction(context, function));
    SyntaxAnalyzer.INSTANCE.analyzeFormsWithFunctions(context, list, 2, getFunctionSymbols(functionList));
  }

  private void analyzeFunction(AnalyzerContext context, LispSexp function) {
    if (function.getList() != null && function.getList().getSexpList().size() >= 2) {
      analyzeLambda(context, type.name(), function.getList(), 1);
    }
  }

  /**
   * Quick scan to find the name of the lexical functions. Errors are ignored, annotations will happen at a later
   * stage, when each function is analyzed.
   */
  private Collection<LexicalSymbol> getFunctionSymbols(@NotNull List<LispSexp> functionList) {
    List<LexicalSymbol> result = new ArrayList<>();
    for (LispSexp function : functionList) {
      if (function.getList() == null || function.getList().getSexpList().size() < 2) {
        function.setErrorMessage("Function definition expected");
        continue;
      }
      LispSexp functionName = function.getList().getSexpList().get(0);
      if (functionName.getSymbol() == null) {
        function.setErrorMessage("Function name expected");
        continue;
      }
      result.add(newLexicalFunction(functionName.getSymbolName()));
    }
    return result;
  }

  public enum Type {
    FLET,
    LABELS,
    MACROLET
  }
}
