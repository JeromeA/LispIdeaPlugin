package org.ax1.lisp.analysis.symbol;

import org.ax1.lisp.analysis.LocatedSymbol;
import org.ax1.lisp.psi.LispSexp;

import java.util.ArrayList;
import java.util.List;

public class SymbolDefinition extends LispDefinition {
  public final Symbol symbol;
  public final List<LispSexp> methods = new ArrayList<>(); // For the methods of a generic.
  public final Type type;
  public final Scope scope;
  public String description; // Whatever should be in the tooltip.
  public LispSexp container;

  private SymbolDefinition(Type type, Scope scope, Symbol symbol, String description) {
    this.symbol = symbol;
    this.type = type;
    this.scope = scope;
    this.description = description;
  }

  public static SymbolDefinition newDefinition(Type type, Scope scope, Symbol symbol, LispSexp definition, String description) {
    SymbolDefinition symbolDefinition = new SymbolDefinition(type, scope, symbol, description);
    symbolDefinition.definitions.add(definition);
    return symbolDefinition;
  }

  public static SymbolDefinition newDefinition(Type type, Scope scope, LocatedSymbol locatedSymbol, String description) {
    return newDefinition(type, scope, locatedSymbol.symbol, locatedSymbol.location, description);
  }

  public static SymbolDefinition newDefinition(Type type, Scope scope, Symbol symbol, String description) {
    return new SymbolDefinition(type, scope, symbol, description);
  }

  public static SymbolDefinition newMethod(Symbol symbol, LispSexp definition, String description) {
    SymbolDefinition symbolDefinition = new SymbolDefinition(Type.FUNCTION, Scope.DYNAMIC, symbol, description);
    symbolDefinition.methods.add(definition);
    return symbolDefinition;
  }

  public static SymbolDefinition newUsage(Type type, Scope scope, Symbol symbol, LispSexp usage) {
    SymbolDefinition symbolDefinition = new SymbolDefinition(type, scope, symbol, null);
    symbolDefinition.usages.add(usage);
    return symbolDefinition;
  }

  @Override
  public String getName() {
    return symbol.getName();
  }

  public enum Type {
    FUNCTION,
    VARIABLE
  }

  public enum Scope {
    DYNAMIC,
    LEXICAL,
  }
}
