package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalyzerContext;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;

import java.util.List;

import static org.ax1.lisp.psi.LispTypes.STRING;

public class AnalyzeDefstruct implements FormAnalyzer {

  @Override
  public void analyze(AnalyzerContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      form.setErrorMessage("DEFSTRUCT needs at least 1 argument");
      return;
    }

    LispSexp structName = list.get(1);
    Struct struct = createStruct(structName);
    if (struct == null) return;
    struct.symbolName.addFunctionDefinition("MAKE-" + struct.name, context.packageContext);

    // Skip documentation.
    int arg = 2;
    if (list.size() > arg && list.get(arg).getFirstChild().getNode().getElementType() == STRING) arg++;

    while (list.size() > arg) {
      analyzeSlot(context, struct, list.get(arg));
      arg++;
    }
  }

  private Struct createStruct(LispSexp nameSexp) {
    LispSymbolName symbolName;
    if (nameSexp.isSymbol()) {
      symbolName = nameSexp.getSymbolName();
    } else if (nameSexp.getList() == null
        || nameSexp.getList().getSexpList().isEmpty()
        || nameSexp.getList().getSexpList().get(0).getSymbol() == null) {
      nameSexp.setErrorMessage("Struct name expected");
      return null;
    } else {
      symbolName = nameSexp.getList().getSexpList().get(0).getSymbolName();
    }
    return new Struct(symbolName);
  }

  private void analyzeSlot(AnalyzerContext context, Struct struct, LispSexp slot) {
    if (slot.getSymbol() != null) {
      slot.getSymbolName().addFunctionDefinition(struct.name + "-" + slot.getText(), context.packageContext);
      return;
    }
    LispList list = slot.getList();
    if (list != null && list.getSexpList().size() >= 1 && list.getSexpList().get(0).isSymbol()) {
      LispSymbolName symbolName = list.getSexpList().get(0).getSymbolName();
      symbolName.addFunctionDefinition(struct.name + "-" + symbolName.getLispName(), context.packageContext);
      // TODO: analyze slot options.
      return;
    }
    slot.setErrorMessage("Slot definition expected");
  }

  public static class Struct {
    public final LispSymbolName symbolName;
    public final String name;
    // TODO: add slots maybe?.

    public Struct(LispSymbolName symbolName) {
      this.symbolName = symbolName;
      this.name = symbolName.getName();
    }
  }
}
