package org.ax1.lisp.analysis.symbol;

import org.ax1.lisp.SymbolResolver;
import org.ax1.lisp.psi.LispSymbolName;

import java.util.HashSet;
import java.util.Set;

import static org.ax1.lisp.analysis.BaseLispElement.Type.LEXICAL_VARIABLE_DEFINITION;

public class LexicalSymbol {
  public LispSymbolName definition;
  public Set<LispSymbolName> usages = new HashSet<>();
  public Symbol symbol;

  public LexicalSymbol(LispSymbolName definition) {
    this.definition = definition;
    this.symbol = SymbolResolver.resolve(definition);
    definition.setType(LEXICAL_VARIABLE_DEFINITION);
    definition.setLexicalVariable(this);
  }
}
