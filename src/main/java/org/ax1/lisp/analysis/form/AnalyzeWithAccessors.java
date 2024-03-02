package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalyzerContext;
import org.ax1.lisp.analysis.BaseLispElement;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LexicalSymbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;

import java.util.ArrayList;
import java.util.List;

public class AnalyzeWithAccessors implements FormAnalyzer {

  @Override
  public void analyze(AnalyzerContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      form.setErrorMessage("WITH-ACCESSORS needs at least 2 argument: (slot-entry*) instance-form");
      return;
    }

    LispList slotEntryList = list.get(1).getList();
    if (slotEntryList == null) {
      list.get(1).setErrorMessage("(slot-entry*) list expected");
      return;
    }
    List<LexicalSymbol> variables = new ArrayList<>();
    for (LispSexp slotEntry : slotEntryList.getSexpList()) {
      LispSymbolName symbolName = slotEntry.getSymbolName();
      if (symbolName != null) {
        // This is not accurate: we need this symbol to be both a SLOT_USAGE and a LEXICAL_VARIABLE_DEFINITION, but
        // there is no way to do this for now, and the lexical variable highlighting is locally more important.
        variables.add(LexicalSymbol.newLexicalVariable(symbolName));
        continue;
      }
      LispList slotEntryPair = slotEntry.getList();
      if (slotEntryPair == null || slotEntryPair.getSexpList().size() != 2) {
        slotEntry.setErrorMessage("Slot entry expected: (variable-name accessor-name) or accessor-name");
        continue;
      }
      List<LispSexp> slotEntryPairList = slotEntryPair.getSexpList();
      LispSymbolName variable = slotEntryPairList.get(0).getSymbolName();
      if (variable == null) {
        slotEntryPairList.get(0).setErrorMessage("variable-name expected");
        continue;
      }
      variables.add(LexicalSymbol.newLexicalVariable(variable));
      LispSymbolName accessor = slotEntryPairList.get(1).getSymbolName();
      if (accessor == null) {
        slotEntryPairList.get(1).setErrorMessage("accessor-name expected");
        continue;
      }
      accessor.setType(BaseLispElement.Type.SLOT_USAGE, context.packageContext);
    }

    SyntaxAnalyzer.INSTANCE.analyzeForm(context, list.get(2));
    SyntaxAnalyzer.INSTANCE.analyzeFormsWithVariables(context, list, 3, variables);
  }
}
