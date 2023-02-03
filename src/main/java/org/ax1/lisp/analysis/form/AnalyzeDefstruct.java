package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;

import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.FUNCTION_DECLARATION;
import static org.ax1.lisp.psi.LispTypes.STRING;

public class AnalyzeDefstruct implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      context.highlighter.highlightError(form, "DEFSTRUCT needs at least 1 argument");
      return;
    }

    LispSexp structName = list.get(1);
    Struct struct = createStruct(context, structName);
    if (struct == null) return;
    Symbol symbol = context.getSymbol(struct.fullSymbolName, "MAKE-" + struct.symbol.getName());
    context.result.addFunctionDefinition(symbol, struct.fullSymbolName.getSymbolName());

    // Skip documentation.
    int arg = 2;
    if (list.size() > arg && list.get(arg).getFirstChild().getNode().getElementType() == STRING) arg++;

    while (list.size() > arg) {
      analyzeSlot(context, struct, list.get(arg));
      arg++;
    }
  }

  private Struct createStruct(AnalysisContext context, LispSexp nameSexp) {
    if (nameSexp.isSymbol()) {
      return createStruct(context, nameSexp);
    }
    if (nameSexp.getList() == null
        || nameSexp.getList().getSexpList().isEmpty()
        || nameSexp.getList().getSexpList().get(0).getSymbol() == null) {
      context.highlighter.highlightError(nameSexp, "Struct name expected");
      return null;
    }
    return newStruct(context, nameSexp.getList().getSexpList().get(0).getSymbol());
  }

  private Struct newStruct(AnalysisContext context, LispSymbol fullSymbolName) {
    Symbol symbol = context.getSymbol(fullSymbolName);
    return new Struct(fullSymbolName, symbol);
  }

  private void analyzeSlot(AnalysisContext context, Struct struct, LispSexp slot) {
    if (slot.getSymbol() != null) {
      Symbol functionSymbol = context.getSymbol(slot.getSymbol(), struct.symbol.getName() + "-" + slot.getText());
      context.result.addFunctionDefinition(functionSymbol, slot.getSymbolName());
      context.highlighter.highlight(slot, FUNCTION_DECLARATION);
      return;
    }
    LispList list = slot.getList();
    if (list != null && list.getSexpList().size() >= 1 && list.getSexpList().get(0).isSymbol()) {
      LispSexp sexp = list.getSexpList().get(0);
      Symbol functionSymbol = context.getSymbol(sexp.getSymbol(), struct.symbol.getName() + "-" + sexp.getText());
      context.result.addFunctionDefinition(functionSymbol, sexp.getSymbolName());
      context.highlighter.highlight(sexp, FUNCTION_DECLARATION);
      // TODO: analyze slot options.
      return;
    }
    context.highlighter.highlightError(slot, "Slot definition expected");
  }

  public static class Struct {
    public final LispSymbol fullSymbolName;
    public final Symbol symbol;

    public Struct(LispSymbol fullSymbolName, Symbol symbol) {
      this.fullSymbolName = fullSymbolName;
      this.symbol = symbol;
    }
  }
}
