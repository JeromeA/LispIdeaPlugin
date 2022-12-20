package org.ax1.lisp.analysis.symbol;

import org.ax1.lisp.analysis.LocatedSymbol;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.impl.LispStringDesignator;

import java.util.*;

public class SymbolDefinition {
  protected final Set<LispStringDesignator> usages = new HashSet<>();
  protected final Set<LispStringDesignator> definitions = new HashSet<>();
  public final Symbol symbol;
  public final Set<LispStringDesignator> methods = new HashSet<>(); // For the methods of a generic.
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

  public static SymbolDefinition newDefinition(
      Type type, Scope scope, Symbol symbol, LispStringDesignator definition, String description) {
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

  public static SymbolDefinition newMethod(Symbol symbol, LispStringDesignator definition, String description) {
    SymbolDefinition symbolDefinition = new SymbolDefinition(Type.FUNCTION, Scope.DYNAMIC, symbol, description);
    symbolDefinition.methods.add(definition);
    return symbolDefinition;
  }

  public static SymbolDefinition newUsage(Type type, Scope scope, Symbol symbol, LispStringDesignator usage) {
    SymbolDefinition symbolDefinition = new SymbolDefinition(type, scope, symbol, null);
    symbolDefinition.usages.add(usage);
    return symbolDefinition;
  }

  public Set<LispStringDesignator> getDefinitions() {
    return definitions;
  }

  public LispStringDesignator getDefinition() {
    if (definitions.isEmpty()) return null;
    return definitions.stream().findFirst().get();
  }

  public boolean isDefinition(LispStringDesignator symbolName) {
    return definitions.contains(symbolName);
  }

  public Collection<LispStringDesignator> getUsages() {
    return usages;
  }

  public boolean isUsage(LispStringDesignator symbolName) {
    return usages.contains(symbolName);
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
