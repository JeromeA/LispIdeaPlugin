package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LexicalSymbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;

import java.util.List;
import java.util.Set;

public class AnalyzeDoSymbols implements FormAnalyzer {

  private final Type type;

  public AnalyzeDoSymbols(Type type) {
    this.type = type;
  }

  @Override
  public void analyze(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      form.setErrorMessage(type.getName() + " needs at least 1 argument");
      return;
    }
    LispList varList = list.get(1).getList();
    if (varList == null || varList.getSexpList().isEmpty() || varList.getSexpList().size() > 3) {
      list.get(1).setErrorMessage("(var [package [result-form]]) expected");
      return;
    }
    LispSexp varName = varList.getSexpList().get(0);
    if (varName.getSymbol() == null) {
      varList.getSexpList().get(0).setErrorMessage("variable name expected");
      return;
    }
    LispSymbolName symbolName = varName.getSymbolName();
    Set<LexicalSymbol> variables = Set.of(new LexicalSymbol(symbolName));
    SyntaxAnalyzer.INSTANCE.analyzeFormsWithVariables(varList.getSexpList(), 1, variables);
    SyntaxAnalyzer.INSTANCE.analyzeFormsWithVariables(list, 2, variables);
  }

  public enum Type {
    DO_SYMBOLS("DO-SYMBOLS"),
    DO_EXTERNAL_SYMBOLS("DO-EXTERNAL-SYMBOLS"),
    DO_ALL_SYMBOLS("DO-ALL-SYMBOLS");

    private final String name;

    Type(String name) {
      this.name = name;
    }

    public String getName() {
      return name;
    }
  }
}
