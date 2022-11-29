package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

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
    LispSymbol symbol = nameSexp.getSymbol();
    if (symbol == null) {
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
    LispSymbol simpleSymbol = slot.getSymbol();
    if (simpleSymbol != null) {
      return;
    }
    LispList slotList = slot.getList();
    if (slotList == null || slotList.getSexpList().size() < 1 || slotList.getSexpList().get(0).getSymbol() == null) {
      context.highlighter.highlightError(slot, "Slot definition expected");
      return;
    }
    List<LispSexp> slotOptions = slotList.getSexpList();
    for (int i = 1; i < slotOptions.size()-1; i += 2) {
      LispSymbol slotOption = slotOptions.get(i).getSymbol();
      if (slotOption == null) {
        context.highlighter.highlightError(slotOptions.get(i), "Slot option expected");
        continue;
      }
      context.highlighter.highlightKeyword(slotOption);
      Symbol option = context.packageManager.getSymbol(slotOption);
      if (METHOD_GENERATORS.contains(option)) {
        LispSymbol symbolName = slotOptions.get(i + 1).getSymbol();
        if (symbolName == null) {
          context.highlighter.highlightError(slotOptions.get(i + 1), "Method name expected");
          continue;
        }
        context.addFunctionDefinition(symbolName);
        context.highlighter.highlight(symbolName, FUNCTION_DECLARATION);
      } else if (!OTHER_OPTIONS.contains(option)) {
        context.highlighter.highlightError(slotOption, "Slot option expected");
      }
    }
  }
}
