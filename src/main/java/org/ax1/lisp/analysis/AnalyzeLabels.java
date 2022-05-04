package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.LexicalBindingManager.LexicalScope;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

import static org.ax1.lisp.analysis.LambdaAnalyzer.analyzeLambda;

public class AnalyzeLabels implements Analyzer {

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.annotations.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      analyzer.annotations.highlightError(form, "LABELS needs at least 1 argument");
      return;
    }
    LispList list1 = list.get(1).getList();
    if (list1 == null) {
      analyzer.annotations.highlightError(list.get(1), "Function binding list expected");
      return;
    }
    List<LispSexp> functionList = list1.getSexpList();
    try (LexicalScope lexicalScope = analyzer.lexicalBindings.defineLexicalFunctions(form, getFunctionSymbols(analyzer, functionList))) {
      functionList.forEach(function -> analyzeFunction(analyzer, function));
      analyzer.analyzeForms(list, 2);
    }
  }

  private void analyzeFunction(SyntaxAnalyzer analyzer, LispSexp function) {
    if (function.getList() != null && function.getList().getSexpList().size() >= 2) {
      analyzeLambda(analyzer, function.getList(), 1);
    }
  }

  /**
   *  Quick scan to find the name of the lexical functions. Errors are ignored, annotations will happen at a later
   *  stage, when each function is analyzed.
   */
  private List<LispSymbol> getFunctionSymbols(SyntaxAnalyzer analyzer, @NotNull List<LispSexp> functionList) {
    List<LispSymbol> result = new ArrayList<>();
    for (LispSexp function : functionList) {
      if (function.getList() == null || function.getList().getSexpList().size() < 2) {
        analyzer.annotations.highlightError(function, "Function definition expected");
        continue;
      }
      LispSexp functionName = function.getList().getSexpList().get(0);
      if (functionName == null) {
        analyzer.annotations.highlightError(function, "Function name expected");
        continue;
      }
      result.add(functionName.getSymbol());
    }
    return result;
  }

}
