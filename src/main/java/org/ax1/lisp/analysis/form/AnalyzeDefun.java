package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;

import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.FUNCTION_DECLARATION;
import static org.ax1.lisp.analysis.form.LambdaAnalyzer.analyzeLambda;

public class AnalyzeDefun implements FormAnalyzer {

  private final Type type;

  public AnalyzeDefun(Type type) {
    this.type = type;
  }

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.annotations.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      analyzer.annotations.highlightError(form, type.name() + " needs at least 2 arguments");
      return;
    }
    LispSexp sexp1 = list.get(1);
    LispSymbol symbol1 = sexp1.getSymbol();
    if (symbol1 != null) {
      analyzer.symbolManager.getFunction(symbol1.getText()).setDefinition(form, symbol1);
      analyzer.annotations.highlight(symbol1, FUNCTION_DECLARATION);
    } else {
      // TODO: check DEFUN SETF case.
    }
    analyzeLambda(analyzer, form, 2);
  }

  public enum Type {
    DEFUN,
    DEFMACRO
  }
}
