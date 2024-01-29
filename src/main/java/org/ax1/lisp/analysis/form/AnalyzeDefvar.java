package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalyzerContext;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;

import java.util.List;

import static org.ax1.lisp.analysis.BaseLispElement.Type.*;

public class AnalyzeDefvar implements FormAnalyzer {

  private final Type type;

  public AnalyzeDefvar(Type type) {
    this.type = type;
  }

  @Override
  public void analyze(AnalyzerContext context, LispList form) {
    List<LispSexp> sexpList = form.getSexpList();
    if (sexpList.size() < type.getMinArg() + 1) {
      form.setErrorMessage(type.name() + " takes at least 1 argument");
      return;
    }
    LispSexp varName = sexpList.get(1);
    if (varName.getSymbol() == null) {
      varName.setErrorMessage("Variable name expected");
      return;
    }
    LispSymbolName symbolName = varName.getSymbolName();
    symbolName.setType(VARIABLE_DEFINITION, context.packageContext);
    if (sexpList.size() >= 3) SyntaxAnalyzer.INSTANCE.analyzeForm(context, sexpList.get(2));
    // Documentation.
    if (sexpList.size() >= 4) sexpList.get(3).setType(CODE);
    sexpList.stream().skip(4).forEach(sexp -> sexp.setErrorMessage("Unexpected argument"));
  }

//  @NotNull
//  private String getDescription(List<LispSexp> sexpList, String name) {
//    StringBuilder sb = new StringBuilder();
//    sb.append(DEFINITION_ELEMENT.addText("Variable " + name));
//    sb.append(SECTIONS_START);
//    sb.append(SECTION_HEADER_CELL.addText("Binding site:"));
//    sb.append(SECTION_CONTENT_CELL.addText(type.name()));
//    sb.append("</tr>");
//    sb.append(SECTION_HEADER_CELL.addText("Initial value:"));
//    if (sexpList.size() >= 3) {
//      sb.append(SECTION_CONTENT_CELL.addText(sexpList.get(2).getText()));
//    } else {
//      sb.append(SECTION_CONTENT_CELL.addText("unbound"));
//    }
//    sb.append(SECTIONS_END);
//    return sb.toString();
//  }

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
