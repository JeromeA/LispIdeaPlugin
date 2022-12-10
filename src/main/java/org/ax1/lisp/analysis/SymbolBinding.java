package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSymbol;

import java.util.ArrayList;
import java.util.List;

public class SymbolBinding {
  public final Symbol symbol;
  public final List<LispSymbol> definitions = new ArrayList<>();
  public final List<LispSymbol> methods = new ArrayList<>(); // For the methods of a generic.
  public final List<LispSymbol> usages = new ArrayList<>();
  public final Type type;
  public final Scope scope;
  public String description; // Whatever should be in the tooltip.
  public LispList container;

  private SymbolBinding(Type type, Scope scope, Symbol symbol, String description) {
    this.symbol = symbol;
    this.type = type;
    this.scope = scope;
    this.description = description;
  }

  public static SymbolBinding newDefinition(Type type, Scope scope, Symbol symbol, LispSymbol definition, String description) {
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

  public static SymbolBinding newMethod(Symbol symbol, LispSymbol definition, String description) {
    SymbolBinding symbolBinding = new SymbolBinding(Type.FUNCTION, Scope.DYNAMIC, symbol, description);
    symbolBinding.methods.add(definition);
    return symbolBinding;
  }

  public static SymbolBinding newUsage(Type type, Scope scope, Symbol symbol, LispSymbol usage) {
    SymbolBinding symbolBinding = new SymbolBinding(type, scope, symbol, null);
    symbolBinding.usages.add(usage);
    return symbolBinding;
  }

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
