package org.ax1.lisp.analysis.symbol;

import org.ax1.lisp.analysis.LocatedSymbol;
import org.ax1.lisp.psi.LispSexp;

import java.util.ArrayList;
import java.util.List;

public class SymbolBinding extends LispDefinition {
  public final Symbol symbol;
  public final List<LispSexp> methods = new ArrayList<>(); // For the methods of a generic.
  public final Type type;
  public final Scope scope;
  public String description; // Whatever should be in the tooltip.
  public LispSexp container;

  private SymbolBinding(Type type, Scope scope, Symbol symbol, String description) {
    this.symbol = symbol;
    this.type = type;
    this.scope = scope;
    this.description = description;
  }

  public static SymbolBinding newDefinition(Type type, Scope scope, Symbol symbol, LispSexp definition, String description) {
    SymbolBinding symbolBinding = new SymbolBinding(type, scope, symbol, description);
    symbolBinding.definitions.add(definition);
    return symbolBinding;
  }

  public static SymbolBinding newDefinition(Type type, Scope scope, LocatedSymbol locatedSymbol, String description) {
    return newDefinition(type, scope, locatedSymbol.symbol, locatedSymbol.location, description);
  }

  public static SymbolBinding newDefinition(Type type, Scope scope, Symbol symbol, String description) {
    return new SymbolBinding(type, scope, symbol, description);
  }

  public static SymbolBinding newMethod(Symbol symbol, LispSexp definition, String description) {
    SymbolBinding symbolBinding = new SymbolBinding(Type.FUNCTION, Scope.DYNAMIC, symbol, description);
    symbolBinding.methods.add(definition);
    return symbolBinding;
  }

  public static SymbolBinding newUsage(Type type, Scope scope, Symbol symbol, LispSexp usage) {
    SymbolBinding symbolBinding = new SymbolBinding(type, scope, symbol, null);
    symbolBinding.usages.add(usage);
    return symbolBinding;
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
