package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.symbol.Lambda;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispString;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;

import static org.ax1.lisp.analysis.form.LambdaAnalyzer.analyzeLambda;
import static org.ax1.lisp.analysis.symbol.SymbolDefinition.newDefinition;

public class AnalyzeDefun implements FormAnalyzer {

  private final Type type;

  public AnalyzeDefun(Type type) {
    this.type = type;
  }

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      context.highlighter.highlightError(form, type.name() + " needs at least 2 arguments");
      return;
    }
    LispSexp functionName = list.get(1);
    if (functionName.isSymbol()) {
      LispSymbol symbol = functionName.getSymbol();
      SymbolDefinition symbolDefinition = newDefinition(SymbolDefinition.Type.FUNCTION, SymbolDefinition.Scope.DYNAMIC, context.getSymbol(symbol), symbol.getSymbolName());
      context.highlighter.highlightDeclaration(functionName.getSymbolName());
      LispList lambda = list.get(2).getList();
      // No need to check for the null case, it will be handled in analyzeLambda().
      if (lambda != null) {
        symbolDefinition.setLambda(Lambda.from(lambda));
      }
      String docString = getDocString(list);
      if (docString != null) {
        symbolDefinition.setDescriptionString(docString);
      }
      context.result.addDefinition(symbolDefinition);
    } else {
      // TODO: check DEFUN SETF case.
    }
    analyzeLambda("DEFUN", context, form, 2);
  }

  private String getDocString(List<LispSexp> list) {
    if (list.size() < 4) return null;
    LispString string3 = list.get(3).getString();
    if (string3 == null) return null;
    return string3.getStringContent().getValue();
  }

  public enum Type {
    DEFUN,
    DEFMACRO
  }
}
