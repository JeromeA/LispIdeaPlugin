package org.ax1.lisp.loop;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LexicalSymbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.ax1.lisp.analysis.BaseLispElement.Type.*;

public class LoopParserBase {
  private List<LispSexp> sexpList;
  private final List<LispSexp> delayed = new ArrayList<>();
  private int index = 0;
  private LispList form;

  public void init(LispList form) {
    this.form = form;
    this.sexpList = form.getSexpList();
  }

  public void End() {
    delayed.forEach(this::analyzeForm);
  }

  public void skip() {
    sexpList.get(index).setType(CODE);
    index++;
  }

  public void usingHashValueKey() {
    LispList compound = sexpList.get(index).getList();
    if (checkValidCompound(compound)) {
      compound.setType(CODE);
    } else {
      compound.setErrorMessage("(HASH-VALUE var) or (HASH-KEY var) expected");
    }
    index++;
  }

  private boolean checkValidCompound(LispList compound) {
    List<LispSexp> compoundSexpList = compound.getSexpList();
    if (compoundSexpList.size() != 2) return false;
    if (!compoundSexpList.get(0).isSymbol()) return false;
    if (!compoundSexpList.get(1).isSymbol()) return false;
    LispSymbolName symbolName0 = compoundSexpList.get(0).getSymbolName();
    String value0 = symbolName0.getLispName();
    if (! (value0.equals("HASH-VALUE") || value0.equals("HASH-KEY"))) return false;
    symbolName0.setType(CODE);
    LispSymbolName symbolName1 = compoundSexpList.get(1).getSymbolName();
    ((LispSexp)form.getParent()).addLexicalVariables(Set.of(new LexicalSymbol(symbolName1)));
    return true;
  }

  public void keyword() {
    sexpList.get(index).setType(KEYWORD);
    index++;
  }

  public void analyzeForm() {
    analyzeForm(sexpList.get(index));
    index++;
  }

  public void delayedAnalyzeForm() {
    delayed.add(sexpList.get(index));
    index++;
  }

  private void analyzeForm(LispSexp form) {
    SyntaxAnalyzer.INSTANCE.analyzeForm(form);
  }

  public void declareVariable() {
    ((LispSexp)form.getParent()).addLexicalVariables(getVariables(sexpList.get(index)));
    index++;
  }

  private List<LexicalSymbol> getVariables(LispSexp sexp) {
    if (sexp.getSymbol() != null) {
      if (sexp.getText().equals("nil")) {
        sexp.setType(KEYWORD);
        return List.of();
      }
      return List.of(new LexicalSymbol(sexp.getSymbolName()));
    }
    LispList list = sexp.getList();
    if (list != null) {
      List<LispSexp> sexpList = list.getSexpList();
      return sexpList.stream().flatMap(s -> getVariables(s).stream()).collect(Collectors.toList());
    }
    sexp.setErrorMessage("Variable name expected");
    return List.of();
  }

  public void error(String text) {
    sexpList.get(index).setErrorMessage(text);
  }

  public void missingExpression() {
    sexpList.get(index - 1).setErrorMessage("Missing expression");
  }

  public void missingVariable() {
    sexpList.get(index - 1).setErrorMessage("Missing variable name");
  }
}
