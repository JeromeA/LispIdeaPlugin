package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.FUNCTION_DECLARATION;
import static org.ax1.lisp.psi.LispTypes.STRING;

public class AnalyzeDefstruct implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      context.highlighter.highlightError(form, "DEFSTRUCT needs at least 1 argument");
      return;
    }

    Struct struct = createStruct(context, list.get(1));
    if (struct == null) return;
    Symbol symbol = context.packageManager.getSymbol("make-" + struct.name);
    context.result.addFunctionDefinition(symbol, struct.symbolSexp, "");

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
    return newStruct(context, nameSexp.getList().getSexpList().get(0));
  }

  private Struct newStruct(AnalysisContext context, LispSexp sexp) {
    String name = sexp.getText();
    return new Struct(name, sexp, context.packageManager.getSymbol(name));
  }

  private void analyzeSlot(AnalysisContext context, Struct struct, LispSexp slot) {
    if (slot.isSymbol()) {
      Symbol functionSymbol = context.packageManager.getSymbol(struct.name + "-" + slot.getText());
      context.result.addFunctionDefinition(functionSymbol, slot, "");
      context.highlighter.highlight(slot, FUNCTION_DECLARATION);
      return;
    }
    LispList list = slot.getList();
    if (list != null && list.getSexpList().size() >= 1 && list.getSexpList().get(0).isSymbol()) {
      LispSexp sexp = list.getSexpList().get(0);
      Symbol functionSymbol = context.packageManager.getSymbol(struct.name + "-" + sexp.getText());
      context.result.addFunctionDefinition(functionSymbol, sexp, "");
      context.highlighter.highlight(sexp, FUNCTION_DECLARATION);
      // TODO: analyze slot options.
      return;
    }
    context.highlighter.highlightError(slot, "Slot definition expected");
  }

  public static class Struct {
    public final String name;
    public final LispSexp symbolSexp;
    public final Symbol symbol;

    public Struct(String name, LispSexp symbolSexp, Symbol symbol) {
      this.name = name;
      this.symbolSexp = symbolSexp;
      this.symbol = symbol;
    }
  }
}
