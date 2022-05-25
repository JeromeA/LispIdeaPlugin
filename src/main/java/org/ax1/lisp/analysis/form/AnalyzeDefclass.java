package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
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
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.annotations.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 4) {
      analyzer.annotations.highlightError(form, "DEFCLASS needs at least 3 arguments");
      return;
    }

    // Class name
    LispSexp nameSexp = list.get(1);
    LispSymbol symbol = nameSexp.getSymbol();
    if (symbol == null) {
      analyzer.annotations.highlightError(nameSexp, "Class name expected");
      return;
    }

    // Superclasses
    LispSexp superclassSexp = list.get(2);
    LispList superclassList = superclassSexp.getList();
    if (superclassList == null) {
      analyzer.annotations.highlightError(superclassSexp, "Superclass list expected");
      return;
    }

    // Slots
    LispSexp slotSexp = list.get(3);
    LispList slotList = slotSexp.getList();
    if (slotList == null) {
      analyzer.annotations.highlightError(slotSexp, "Slot list expected");
      return;
    }
    slotList.getSexpList().forEach(slot -> analyzeSlot(analyzer, slot));
  }

  private void analyzeSlot(SyntaxAnalyzer analyzer, LispSexp slot) {
    LispSymbol simpleSymbol = slot.getSymbol();
    if (simpleSymbol != null) {
      return;
    }
    LispList slotList = slot.getList();
    if (slotList == null || slotList.getSexpList().size() < 1 || slotList.getSexpList().get(0).getSymbol() == null) {
      analyzer.annotations.highlightError(slot, "Slot definition expected");
      return;
    }
    List<LispSexp> slotOptions = slotList.getSexpList();
    for (int i = 1; i < slotOptions.size()-1; i += 2) {
      LispSymbol slotOption = slotOptions.get(i).getSymbol();
      if (slotOption == null) {
        analyzer.annotations.highlightError(slotOptions.get(i), "Slot option expected");
        continue;
      }
      analyzer.annotations.highlightKeyword(slotOption);
      Symbol option = analyzer.packageManager.getSymbol(slotOption);
      if (METHOD_GENERATORS.contains(option)) {
        LispSymbol nameSymbol = slotOptions.get(i + 1).getSymbol();
        if (nameSymbol == null) {
          analyzer.annotations.highlightError(slotOptions.get(i + 1), "Method name expected");
          continue;
        }
        Symbol methodName = analyzer.packageManager.getSymbol(nameSymbol);
        analyzer.packageManager.getFunction(methodName).setDefinition(null, nameSymbol);
        analyzer.annotations.highlight(nameSymbol, FUNCTION_DECLARATION);
      } else if (!OTHER_OPTIONS.contains(option)) {
        analyzer.annotations.highlightError(slotOption, "Slot option expected");
      }
    }
  }
}
