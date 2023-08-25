package org.ax1.lisp.analysis.symbol;

import org.ax1.lisp.SymbolResolver;
import org.ax1.lisp.psi.LispSymbolName;

import java.util.HashSet;
import java.util.Set;

import static org.ax1.lisp.analysis.BaseLispElement.Type.LEXICAL_FUNCTION_DEFINITION;
import static org.ax1.lisp.analysis.BaseLispElement.Type.LEXICAL_VARIABLE_DEFINITION;

public class LexicalSymbol {
  public LispSymbolName definition;
  public Set<LispSymbolName> usages = new HashSet<>();
  public Symbol symbol;

  private LexicalSymbol(LispSymbolName definition) {
    this.definition = definition;
    this.symbol = SymbolResolver.resolve(definition);
  }

  public static LexicalSymbol newLexicalVariable(LispSymbolName definition) {
    LexicalSymbol lexicalVariable = new LexicalSymbol(definition);
    definition.setType(LEXICAL_VARIABLE_DEFINITION);
    definition.setLexicalVariable(lexicalVariable);
    return lexicalVariable;
  }

  public static LexicalSymbol newLexicalFunction(LispSymbolName definition) {
    LexicalSymbol lexicalFunction = new LexicalSymbol(definition);
    definition.setType(LEXICAL_FUNCTION_DEFINITION);
    definition.setLexicalFunction(lexicalFunction);
    return lexicalFunction;
  }
}
