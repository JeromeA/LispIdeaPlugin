package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalyzerContext;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispString;

import java.util.List;

import static org.ax1.lisp.analysis.form.LambdaAnalyzer.analyzeLambda;

public class AnalyzeDefun implements FormAnalyzer {

  private final Type type;

  public AnalyzeDefun(Type type) {
    this.type = type;
  }

  @Override
  public void analyze(AnalyzerContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      form.setErrorMessage(type.name() + " needs at least 2 arguments");
      return;
    }
    LispSexp functionName = list.get(1);
    if (functionName.isSymbol()) {
      functionName.getSymbolName().addFunctionDefinition(functionName.getSymbolName().getText(), context.packageContext);
    } else {
      // TODO: check DEFUN SETF case.
    }
    analyzeLambda(context, "DEFUN", form, 2);
  }

  private String getDocString(List<LispSexp> list) {
    if (list.size() < 4) return null;
    LispString string3 = list.get(3).getString();
    if (string3 == null) return null;
    return string3.getStringContent().getLispName();
  }

  public enum Type {
    DEFUN,
    DEFMACRO
  }
}
