package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalyzerContext;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;

import java.util.List;
import java.util.Set;

import static org.ax1.lisp.analysis.BaseLispElement.Type.*;

public class AnalyzeDefclass implements FormAnalyzer {

  private static final Set<String> METHOD_GENERATORS = Set.of("READER", "WRITER", "ACCESSOR");
  private static final Set<String> OTHER_OPTIONS = Set.of("ALLOCATION", "INITARG", "INITFORM", "TYPE", "DOCUMENTATION");

  @Override
  public void analyze(AnalyzerContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 4) {
      form.setErrorMessage("DEFCLASS needs at least 3 arguments: name, superclasses, slots");
      return;
    }

    // Class name
    LispSexp nameSexp = list.get(1);
    if (!nameSexp.isSymbol()) {
      nameSexp.setErrorMessage("Class name expected");
      return;
    }
    nameSexp.getSymbolName().setType(CLASS_DEFINITION, context.packageContext);

    // Superclasses
    LispSexp superclassSexp = list.get(2);
    LispList superclassList = superclassSexp.getList();
    if (superclassList == null) {
      superclassSexp.setErrorMessage("Superclass list expected");
      return;
    }
    superclassList.getSexpList().forEach(superclass -> {
      if (superclass.isSymbol()) {
        superclass.getSymbolName().setType(CLASS_USAGE, context.packageContext);
      } else {
        superclass.setErrorMessage("Superclass expected");
      }
    });

    // Slots
    LispSexp slotSexp = list.get(3);
    LispList slotList = slotSexp.getList();
    if (slotList == null) {
      slotSexp.setErrorMessage("Slot list expected");
      return;
    }
    slotList.getSexpList().forEach(slot -> analyzeSlot(context, slot));
  }

  private void analyzeSlot(AnalyzerContext context, LispSexp slot) {
    if (slot.isSymbol()) {
      slot.getSymbolName().setType(SLOT_DEFINITION, context.packageContext);
      return;
    }

    LispList slotList = slot.getList();
    if (slotList == null || slotList.getSexpList().size() < 1 || !slotList.getSexpList().get(0).isSymbol()) {
      slot.setErrorMessage("Slot definition expected");
      return;
    }
    slotList.getSexpList().get(0).getSymbolName().setType(SLOT_DEFINITION, context.packageContext);
    List<LispSexp> slotOptions = slotList.getSexpList();
    for (int i = 1; i < slotOptions.size(); i += 2) {
      LispSexp slotOption = slotOptions.get(i);
      if (slotOption.getSymbol() == null) {
        slotOption.setErrorMessage("Slot option expected");
        continue;
      }
      String slotOptionName = slotOption.getSymbolName().getLispName();
      if (!METHOD_GENERATORS.contains(slotOptionName) && !OTHER_OPTIONS.contains(slotOptionName)) {
        slotOption.setErrorMessage("Unknown slot option");
        continue;
      }
      if (i + 1 >= slotOptions.size()) {
        slotOption.setErrorMessage("Missing slot option value");
        continue;
      }
      LispSexp slotOptionArg = slotOptions.get(i + 1);
      if (METHOD_GENERATORS.contains(slotOptionName)) {
        slotOption.setType(KEYWORD);
        if (slotOptionArg.getSymbol() == null) {
          slotOptionArg.setErrorMessage("Method name expected");
          continue;
        }
        slotOptionArg.getSymbolName().setType(FUNCTION_DEFINITION, context.packageContext);
      } else if (OTHER_OPTIONS.contains(slotOptionName)) {
        slotOption.setType(KEYWORD);
        if (slotOptionName.equals("INITFORM")) {
          SyntaxAnalyzer.INSTANCE.analyzeForm(context, slotOptionArg);
        }
        if (slotOptionName.equals("INITARG")) {
          LispSymbolName symbolName = slotOptionArg.getSymbolName();
          if (symbolName == null) {
            slotOptionArg.setErrorMessage("Symbol name expected");
            continue;
          }
          symbolName.setType(DATA);
        }
      }
    }
  }
}
