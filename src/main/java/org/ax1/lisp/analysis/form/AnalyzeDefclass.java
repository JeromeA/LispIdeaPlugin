package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.ax1.lisp.psi.LispSymbolName;

import java.util.List;
import java.util.Set;

import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.FUNCTION_DECLARATION;
import static org.ax1.lisp.analysis.symbol.Symbol.keywordSymbol;

public class AnalyzeDefclass implements FormAnalyzer {

  private static final Set<Symbol> METHOD_GENERATORS =
      Set.of(keywordSymbol("READER"), keywordSymbol("WRITER"), keywordSymbol("ACCESSOR"));
  private static final Set<Symbol> OTHER_OPTIONS = Set.of(keywordSymbol("ALLOCATION"), keywordSymbol("INITARG"),
      keywordSymbol("INITFORM"), keywordSymbol("TYPE"), keywordSymbol("DOCUMENTATION"));

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 4) {
      context.highlighter.highlightError(form, "DEFCLASS needs at least 3 arguments");
      return;
    }

    // Class name
    LispSexp nameSexp = list.get(1);
    if (!nameSexp.isSymbol()) {
      context.highlighter.highlightError(nameSexp, "Class name expected");
      return;
    }

    // Superclasses
    LispSexp superclassSexp = list.get(2);
    LispList superclassList = superclassSexp.getList();
    if (superclassList == null) {
      context.highlighter.highlightError(superclassSexp, "Superclass list expected");
      return;
    }

    // Slots
    LispSexp slotSexp = list.get(3);
    LispList slotList = slotSexp.getList();
    if (slotList == null) {
      context.highlighter.highlightError(slotSexp, "Slot list expected");
      return;
    }
    slotList.getSexpList().forEach(slot -> analyzeSlot(context, slot));
  }

  private void analyzeSlot(AnalysisContext context, LispSexp slot) {
    if (slot.isSymbol()) {
      return;
    }
    LispList slotList = slot.getList();
    if (slotList == null || slotList.getSexpList().size() < 1 || !slotList.getSexpList().get(0).isSymbol()) {
      context.highlighter.highlightError(slot, "Slot definition expected");
      return;
    }
    List<LispSexp> slotOptions = slotList.getSexpList();
    for (int i = 1; i < slotOptions.size()-1; i += 2) {
      LispSexp slotOption = slotOptions.get(i);
      if (slotOption.getSymbol() == null) {
        context.highlighter.highlightError(slotOptions.get(i), "Slot option expected");
        continue;
      }
      LispSymbol slotOptionName = slotOption.getSymbol();
      Symbol option = context.getSymbol(slotOptionName);
      if (METHOD_GENERATORS.contains(option)) {
        context.highlighter.highlightKeyword(slotOption);
        LispSexp name = slotOptions.get(i + 1);
        if (name.getSymbol() == null) {
          context.highlighter.highlightError(slotOptions.get(i + 1), "Method name expected");
          continue;
        }
        context.addFunctionDefinition(name.getSymbol(), "");
        context.highlighter.highlight(name, FUNCTION_DECLARATION);
      } else if (!OTHER_OPTIONS.contains(option)) {
        context.highlighter.highlightError(slotOption, "Slot option expected");
      }
    }
  }
}
