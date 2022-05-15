package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;

import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.FUNCTION_DECLARATION;
import static org.ax1.lisp.psi.LispTypes.STRING;

public class AnalyzeDefstruct implements FormAnalyzer {

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.annotations.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      analyzer.annotations.highlightError(form, "DEFSTRUCT needs at least 1 argument");
      return;
    }
    LispSymbol symbol = list.get(1).getSymbol();
    if (symbol == null) {
      // TODO: support list format.
      analyzer.annotations.highlightError(list.get(1), "Struct name expected");
      return;
    }
    analyzer.annotations.highlight(symbol, FUNCTION_DECLARATION);
    String structName = symbol.getText();
    analyzer.packageManager.getFunction("make-" + structName).setDefinition(form, symbol);

    // Skip documentation.
    int arg = 2;
    if (list.size() > arg && list.get(arg).getFirstChild().getNode().getElementType() == STRING) arg++;

    while (list.size() > arg) {
      analyzeSlot(analyzer, form, structName, list.get(arg));
      arg++;
    }
  }

  private void analyzeSlot(SyntaxAnalyzer analyzer, LispList container, String structName, LispSexp slot) {
    LispSymbol simpleSymbol = slot.getSymbol();
    if (simpleSymbol != null) {
      analyzer.packageManager.getFunction(structName + "-" + simpleSymbol.getText()).setDefinition(container, simpleSymbol);
      analyzer.annotations.highlight(simpleSymbol, FUNCTION_DECLARATION);
      return;
    }
    LispList list = slot.getList();
    if (list != null && list.getSexpList().size() >= 1 && list.getSexpList().get(0).getSymbol() != null) {
      LispSymbol symbol = list.getSexpList().get(0).getSymbol();
      analyzer.packageManager.getFunction(structName + "-" + symbol.getText()).setDefinition(container, symbol);
      analyzer.annotations.highlight(symbol, FUNCTION_DECLARATION);
      // TODO: analyze slot options.
      return;
    }
    analyzer.annotations.highlightError(slot, "Slot definition expected");
  }
}
