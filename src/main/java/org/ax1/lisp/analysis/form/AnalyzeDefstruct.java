package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.Struct;
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

    Struct struct = createStruct(analyzer, list.get(1));
    if (struct == null) return;
    analyzer.packageManager.getFunction("make-" + struct.name).setDefinition(form, struct.symbolName);

    // Skip documentation.
    int arg = 2;
    if (list.size() > arg && list.get(arg).getFirstChild().getNode().getElementType() == STRING) arg++;

    while (list.size() > arg) {
      analyzeSlot(analyzer, form, struct.name, list.get(arg));
      arg++;
    }
  }

  private Struct createStruct(SyntaxAnalyzer analyzer, LispSexp nameSexp) {
    LispSymbol symbol = nameSexp.getSymbol();
    if (symbol != null) {
      return createStruct(symbol);
    }
    if (nameSexp.getList() == null
        || nameSexp.getList().getSexpList().isEmpty()
        || nameSexp.getList().getSexpList().get(0).getSymbol() == null) {
      analyzer.annotations.highlightError(nameSexp, "Struct name expected");
      return null;
    }
    return createStruct(nameSexp.getList().getSexpList().get(0).getSymbol());
  }

  private Struct createStruct(LispSymbol symbol) {
    Struct struct = new Struct();
    struct.name = symbol.getText();
    struct.symbolName = symbol;
    return struct;
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
