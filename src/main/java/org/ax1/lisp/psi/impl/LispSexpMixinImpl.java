package org.ax1.lisp.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.lang.annotation.AnnotationHolder;
import org.ax1.lisp.analysis.symbol.LexicalSymbol;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;
import org.jetbrains.annotations.NotNull;

import java.util.*;

public abstract class LispSexpMixinImpl extends BaseMixinImpl implements LispSexp {

  private final Map<Symbol, LexicalSymbol> lexicalVariables = new HashMap<>();
  private final Map<Symbol, LexicalSymbol> lexicalFunctions = new HashMap<>();

  public LispSexpMixinImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public boolean isSymbol() {
    return getSymbol() != null;
  }

  @Override
  public LispStringDesignator getStringDesignator() {
    if (getString() != null) {
      return getString().getStringContent();
    }
    return getSymbolName();
  }

  @Override
  public void setType(Type type) {
    // This is only a shortcut to mark a list or a symbol as data, or error, without bothering about what they are.
    if (getSymbolName() != null) {
      getSymbolName().setType(type);
    }
    if (getList() != null) {
      getList().setType(type);
    }
    if (getQuoted() != null) {
      getQuoted().getSexp().setType(type);
    }
    if (getString() != null) {
      getString().getStringContent().setType(type);
    }
  }

  @Override
  public void setErrorMessage(String errorMessage) {
    super.setErrorMessage(errorMessage);
    if (getList() != null) {
      getList().setType(Type.ERROR);
    }
  }

  @Override
  public Type getType() {
    throw new RuntimeException("A Sexp doesn't have a type.");
  }

  @Override
  protected void annotateWithType(@NotNull AnnotationHolder holder) {
    // Nothing to annotate, only errorMessage is a valid annotation.
  }

  @Override
  public LispSymbolName getSymbolName() {
    return getSymbol() == null ? null : getSymbol().getSymbolName();
  }

  @Override
  public void addLexicalVariables(Collection<LexicalSymbol> variables) {
    variables.forEach(variable -> lexicalVariables.put(variable.symbol, variable));
  }

  @Override
  public void addLexicalFunctions(Collection<LexicalSymbol> functions) {
    functions.forEach(function -> lexicalFunctions.put(function.symbol, function));
  }

  @Override
  public Map<Symbol, LexicalSymbol> getLexicalVariables() {
    return lexicalVariables;
  }
}
