package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.LexicalVariableHelper;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

public class AnalyzeDoSymbols implements FormAnalyzer {

  private Type type;

  public AnalyzeDoSymbols(Type type) {
    this.type = type;
  }

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      context.highlighter.highlightError(form, type.getName() + " needs at least 1 argument");
      return;
    }
    LispList varList = list.get(1).getList();
    if (varList == null || varList.getSexpList().isEmpty() || varList.getSexpList().size() > 3) {
      context.highlighter.highlightError(list.get(1), "(var [package [result-form]]) expected");
      return;
    }
    LispSexp varName = varList.getSexpList().get(0);
    if (varName.getSymbol() == null) {
      context.highlighter.highlightError(varList.getSexpList().get(0), "variable name expected");
      return;
    }
    SymbolDefinition binding = LexicalVariableHelper.newLexicalVariable(type.getName(),
        context.getLocatedSymbol(varName.getSymbol()), null);
    context.lexicalBindings.defineLexicalVariables(List.of(binding));
    context.analyzer.analyzeForms(varList.getSexpList(), 1);
    context.analyzer.analyzeForms(list, 2);
    context.lexicalBindings.dropLexicalVariables();
  }

  public enum Type {
    DO_SYMBOLS("DO-SYMBOLS"),
    DO_EXTERNAL_SYMBOLS("DO-EXTERNAL-SYMBOLS"),
    DO_ALL_SYMBOLS("DO-ALL-SYMBOLS");

    private String name;

    Type(String name) {
      this.name = name;
    }

    public String getName() {
      return name;
    }
  }
}
