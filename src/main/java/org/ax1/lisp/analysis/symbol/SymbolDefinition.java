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
  private String description; // Whatever should be in the tooltip.
  private String lambda;
  public LispSexp container;
  public boolean hasExternalDefinition;

  private SymbolDefinition(Type type, Scope scope, Symbol symbol) {
    this.symbol = symbol;
    this.type = type;
    this.scope = scope;
  }

  public static SymbolDefinition newDefinition(
      Type type, Scope scope, Symbol symbol, LispStringDesignator definition) {
    SymbolDefinition symbolDefinition = new SymbolDefinition(type, scope, symbol);
    symbolDefinition.definitions.add(definition);
    return symbolDefinition;
  }

  public static SymbolDefinition newDefinition(Type type, Scope scope, LocatedSymbol locatedSymbol) {
    return newDefinition(type, scope, locatedSymbol.symbol, locatedSymbol.location);
  }

  public static SymbolDefinition newDefinition(Type type, Scope scope, Symbol symbol) {
    return new SymbolDefinition(type, scope, symbol);
  }

  public static SymbolDefinition newMethod(Symbol symbol, LispStringDesignator definition) {
    SymbolDefinition symbolDefinition = new SymbolDefinition(Type.FUNCTION, Scope.DYNAMIC, symbol);
    symbolDefinition.methods.add(definition);
    return symbolDefinition;
  }

  public static SymbolDefinition newUsage(Type type, Scope scope, Symbol symbol, LispStringDesignator usage) {
    SymbolDefinition symbolDefinition = new SymbolDefinition(type, scope, symbol);
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

  public void setDescription(String description) {
    this.description = description;
  }

  public String getDescription() {
    return description;
  }

  public void setLambda(String lambda) {
    this.lambda = lambda;
  }

  public boolean isDefined() {
    return !definitions.isEmpty() || hasExternalDefinition;
  }

  public SymbolDefinition merge(SymbolDefinition def2) {
    getDefinitions().addAll(def2.getDefinitions());
    methods.addAll(def2.methods);
    getUsages().addAll(def2.getUsages());
    if (def2.description != null) description = def2.description;
    if (def2.hasExternalDefinition) hasExternalDefinition = true;
    if (def2.lambda != null) lambda = def2.lambda;
    return this;
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
