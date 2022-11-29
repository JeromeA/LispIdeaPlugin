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
  public final String signature; // Signature of the function, if it's a function definition.
  public final Type type;
  public final Scope scope;
  public String description; // Whatever should be in the tooltip.
  public LispList container;

  private SymbolBinding(Type type, Scope scope, Symbol symbol) {
    this.symbol = symbol;
    this.type = type;
    this.scope = scope;
    signature = null;
    description = null;
  }

  public static SymbolBinding newDefinition(Type type, Scope scope, Symbol symbol, LispSymbol definition) {
    SymbolBinding symbolBinding = new SymbolBinding(type, scope, symbol);
    symbolBinding.definitions.add(definition);
    return symbolBinding;
  }

  public static SymbolBinding newDefinition(Type type, Scope scope, LocatedSymbol locatedSymbol) {
    return newDefinition(type, scope, locatedSymbol.symbol, locatedSymbol.location);
  }

  public static SymbolBinding newDefinition(Type type, Scope scope, Symbol symbol, String description) {
    SymbolBinding symbolBinding = new SymbolBinding(type, scope, symbol);
    symbolBinding.description = description;
    return symbolBinding;
  }

  public static SymbolBinding newMethod(Symbol symbol, LispSymbol definition) {
    SymbolBinding symbolBinding = new SymbolBinding(Type.METHOD, Scope.DYNAMIC, symbol);
    symbolBinding.methods.add(definition);
    return symbolBinding;
  }

  public static SymbolBinding newUsage(Type type, Scope scope, Symbol symbol, LispSymbol usage) {
    SymbolBinding symbolBinding = new SymbolBinding(type, scope, symbol);
    symbolBinding.usages.add(usage);
    return symbolBinding;
  }

  public String getName() {
    return symbol.getName();
  }

  public enum Type {
    FUNCTION,
    METHOD,
    VARIABLE
  }

  public enum Scope {
    DYNAMIC,
    LEXICAL,
  }
}
