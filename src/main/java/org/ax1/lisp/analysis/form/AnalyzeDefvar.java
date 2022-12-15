package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;

import java.util.List;

import static com.intellij.lang.documentation.DocumentationMarkup.*;

public class AnalyzeDefvar implements FormAnalyzer {

  private final Type type;

  public AnalyzeDefvar(Type type) {
    this.type = type;
  }

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    List<LispSexp> sexpList = form.getSexpList();
    if (sexpList.size() < type.getMinArg() + 1) {
      context.highlighter.highlightError(form, type.name() + " takes at least 1 argument");
      return;
    }
    LispSymbol symbol = sexpList.get(1).getSymbol();
    if (symbol == null) {
      context.highlighter.highlightError(sexpList.get(1), "Variable name expected");
      return;
    }
    if (sexpList.size() >= 3) context.analyzer.analyzeForm(sexpList.get(2));
    context.addVariableDefinition(symbol, getDescription(sexpList, symbol));
  }

  @NotNull
  private String getDescription(List<LispSexp> sexpList, LispSymbol symbol) {
    StringBuilder sb = new StringBuilder();
    sb.append(DEFINITION_ELEMENT.addText("Variable " + symbol.getText()));
    sb.append(SECTIONS_START);
    sb.append(SECTION_HEADER_CELL.addText("Binding site:"));
    sb.append(SECTION_CONTENT_CELL.addText(type.name()));
    sb.append("</tr>");
    sb.append(SECTION_HEADER_CELL.addText("Initial value:"));
    if (sexpList.size() >= 3) {
      sb.append(SECTION_CONTENT_CELL.addText(sexpList.get(2).getText()));
    } else {
      sb.append(SECTION_CONTENT_CELL.addText("unbound"));
    }
    sb.append(SECTIONS_END);
    return sb.toString();
  }

  public enum Type {
    DEFCONSTANT(2),
    DEFPARAMETER(2),
    DEFVAR(1);

    final int minArg;

    Type(int minArg) {
      this.minArg = minArg;
    }

    public int getMinArg() {
      return minArg;
    }
  }
}
