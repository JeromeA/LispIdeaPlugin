package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.Symbol;
import org.jetbrains.annotations.NotNull;

import static com.intellij.lang.documentation.DocumentationMarkup.*;
import static org.ax1.lisp.analysis.SymbolBinding.Scope.LEXICAL;
import static org.ax1.lisp.analysis.SymbolBinding.newDefinition;

public class LexicalVariableHelper {

  @NotNull
  public static SymbolBinding newLexicalVariable(String formName, LocatedSymbol locatedSymbol, String initialValue) {
    return newDefinition(SymbolBinding.Type.VARIABLE, LEXICAL, locatedSymbol,
        getDescription(formName, locatedSymbol.symbol, initialValue));
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
