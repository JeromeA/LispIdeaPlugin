package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.jetbrains.annotations.NotNull;

import static com.intellij.lang.documentation.DocumentationMarkup.*;
import static org.ax1.lisp.analysis.symbol.SymbolDefinition.Scope.LEXICAL;
import static org.ax1.lisp.analysis.symbol.SymbolDefinition.newDefinition;

public class LexicalVariableHelper {

  @NotNull
  public static SymbolDefinition newLexicalVariable(String formName, LocatedSymbol locatedSymbol, String initialValue) {
    SymbolDefinition symbolDefinition = newDefinition(SymbolDefinition.Type.VARIABLE, LEXICAL, locatedSymbol);
    symbolDefinition.setDescription(getDescription(formName, locatedSymbol.symbol, initialValue));
    return symbolDefinition;
  }

  @NotNull
  public static String getDescription(String formName, Symbol symbol, String initialValue) {
    StringBuilder sb = new StringBuilder();
    sb.append(DEFINITION_ELEMENT.addText("Variable " + symbol.getName()));
    sb.append(SECTIONS_START);
    sb.append(SECTION_HEADER_CELL.addText("Binding site:"));
    sb.append(SECTION_CONTENT_CELL.addText(formName));
    sb.append("</tr>");
    sb.append(SECTION_HEADER_CELL.addText("Initial value:"));
    sb.append(SECTION_CONTENT_CELL.addText(initialValue == null ? "--" : initialValue));
    sb.append(SECTIONS_END);
    return sb.toString();
  }
}
