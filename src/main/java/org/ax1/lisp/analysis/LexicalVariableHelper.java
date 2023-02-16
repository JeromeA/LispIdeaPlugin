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
    symbolDefinition.setBindingSite(formName);
    symbolDefinition.setInitialValue(initialValue);
    return symbolDefinition;
  }
}
