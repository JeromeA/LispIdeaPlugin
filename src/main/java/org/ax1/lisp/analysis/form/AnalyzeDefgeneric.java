package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.SymbolBinding;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;

import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.FUNCTION_DECLARATION;

public class AnalyzeDefgeneric implements FormAnalyzer {

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.annotations.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      analyzer.annotations.highlightError(form, "DEFGENERIC needs at least 2 arguments");
      return;
    }
    LispSexp sexp1 = list.get(1);
    LispSymbol symbol1 = sexp1.getSymbol();
    if (symbol1 == null) {
      analyzer.annotations.highlightError(sexp1, "Function name expected");
      return;
    }
    SymbolBinding binding = analyzer.packageManager.getFunction(symbol1.getText());
    binding.setDefinition(form, symbol1);
    binding.setGeneric();
    analyzer.annotations.highlight(symbol1, FUNCTION_DECLARATION);
  }
}
