package org.ax1.lisp.analysis.form;

import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;
import java.util.Set;

import static org.ax1.lisp.analysis.BaseLispElement.Type.FUNCTION_DEFINITION;
import static org.ax1.lisp.analysis.BaseLispElement.Type.KEYWORD;

public class AnalyzeDefclass implements FormAnalyzer {

  private static final Set<String> METHOD_GENERATORS = Set.of("READER", "WRITER", "ACCESSOR");
  private static final Set<String> OTHER_OPTIONS = Set.of("ALLOCATION", "INITARG", "INITFORM", "TYPE", "DOCUMENTATION");

  @Override
  public void analyze(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 4) {
      form.setErrorMessage("DEFCLASS needs at least 3 arguments");
      return;
    }

    // Class name
    LispSexp nameSexp = list.get(1);
    if (!nameSexp.isSymbol()) {
      nameSexp.setErrorMessage("Class name expected");
      return;
    }

    // Superclasses
    LispSexp superclassSexp = list.get(2);
    LispList superclassList = superclassSexp.getList();
    if (superclassList == null) {
      superclassSexp.setErrorMessage("Superclass list expected");
      return;
    }

    // Slots
    LispSexp slotSexp = list.get(3);
    LispList slotList = slotSexp.getList();
    if (slotList == null) {
      slotSexp.setErrorMessage("Slot list expected");
      return;
    }
    slotList.getSexpList().forEach(this::analyzeSlot);
  }

  private void analyzeSlot(LispSexp slot) {
    if (slot.isSymbol()) {
      return;
    }
    LispList slotList = slot.getList();
    if (slotList == null || slotList.getSexpList().size() < 1 || !slotList.getSexpList().get(0).isSymbol()) {
      slot.setErrorMessage("Slot definition expected");
      return;
    }
    List<LispSexp> slotOptions = slotList.getSexpList();
    for (int i = 1; i < slotOptions.size()-1; i += 2) {
      LispSexp slotOption = slotOptions.get(i);
      if (slotOption.getSymbol() == null) {
        slotOptions.get(i).setErrorMessage("Slot option expected");
        continue;
      }
      String slotOptionName = slotOption.getSymbolName().getLispName();
      if (METHOD_GENERATORS.contains(slotOptionName)) {
        slotOption.setType(KEYWORD);
        LispSexp name = slotOptions.get(i + 1);
        if (name.getSymbol() == null) {
          slotOptions.get(i + 1).setErrorMessage("Method name expected");
          continue;
        }
        name.getSymbolName().setType(FUNCTION_DEFINITION);
      } else if (!OTHER_OPTIONS.contains(slotOptionName)) {
        slotOption.setErrorMessage("Slot option expected");
      }
    }
  }
}
