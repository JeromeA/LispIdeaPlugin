package org.ax1.lisp.analysis.symbol;

import org.ax1.lisp.analysis.LocatedSymbol;
import org.ax1.lisp.psi.impl.LispStringDesignator;

import java.util.*;

import static com.intellij.lang.documentation.DocumentationMarkup.*;
import static com.intellij.lang.documentation.DocumentationMarkup.SECTIONS_END;

public class SymbolDefinition extends Definition {
  public final Symbol symbol;
  public final Set<LispStringDesignator> methods = new HashSet<>(); // For the methods of a generic.
  public Type type;
  public final Scope scope;
  private Lambda lambda;
  public String bindingSite;
  public String initialValue;
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

  public String getName() {
    return symbol.getName();
  }

  public String getDescription() {
    StringBuilder sb = new StringBuilder();
    sb.append(DEFINITION_ELEMENT.addText(type.getName() + " " + symbol.getQualifiedName()));
    sb.append(SECTIONS_START);
    if (lambda != null) {
      sb.append(SECTION_HEADER_CELL.addText("Lambda:"));
      sb.append(SECTION_CONTENT_CELL.addText(lambda.string));
      sb.append("</tr>");
    }
    if (bindingSite != null) {
      sb.append(SECTION_HEADER_CELL.addText("Binding site:"));
      sb.append(SECTION_CONTENT_CELL.addText(bindingSite));
      sb.append("</tr>");
    }
    if (initialValue != null) {
      sb.append(SECTION_HEADER_CELL.addText("Initial value:"));
      sb.append(SECTION_CONTENT_CELL.addText(initialValue));
      sb.append("</tr>");
    }
    sb.append(SECTION_HEADER_CELL.addText("Documentation:"));
    sb.append(SECTION_CONTENT_CELL.addText(descriptionString == null ? "--" : descriptionString));
    sb.append(SECTIONS_END);
    return sb.toString();
  }

  public SymbolDefinition merge(SymbolDefinition def2) {
    getDefinitions().addAll(def2.getDefinitions());
    methods.addAll(def2.methods);
    getUsages().addAll(def2.getUsages());
    if (def2.descriptionString != null) descriptionString = def2.descriptionString;
    if (def2.hasExternalDefinition) hasExternalDefinition = true;
    if (def2.lambda != null) lambda = def2.lambda;
    type = def2.type;
    return this;
  }

  public void setBindingSite(String bindingSite) {
    this.bindingSite = bindingSite;
  }

  public void setInitialValue(String initialValue) {
    this.initialValue = initialValue;
  }

  public void setLambda(Lambda lambda) {
    this.lambda = lambda;
  }

  public Lambda getLambda() {
    return lambda;
  }

  public enum Type {
    FUNCTION("Function"),
    MACRO("Macro"),
    SPECIAL_OPERATOR("Special operator"),
    VARIABLE("Variable");

    private final String name;

    Type(String name) {
      this.name = name;
    }

    public String getName() {
      return name;
    }
  }

  public enum Scope {
    DYNAMIC,
    LEXICAL,
  }
}
