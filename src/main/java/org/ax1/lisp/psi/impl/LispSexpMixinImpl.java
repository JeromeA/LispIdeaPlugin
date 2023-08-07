package org.ax1.lisp.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.lang.annotation.AnnotationHolder;
import org.ax1.lisp.analysis.symbol.LexicalVariable;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;
import org.jetbrains.annotations.NotNull;

import java.util.*;

public abstract class LispSexpMixinImpl extends BaseMixinImpl implements LispSexp {

  private final Map<Symbol, LexicalVariable> lexicalVariables = new HashMap<>();
  private final Set<LispSymbolName> lexicalFunctions = new HashSet<>();

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
  public void addLexicalVariables(Collection<LexicalVariable> variables) {
    variables.forEach(variable -> lexicalVariables.put(variable.symbol, variable));
  }

  @Override
  public void addLexicalFunctions(Collection<LispSymbolName> functions) {
    lexicalFunctions.addAll(functions);
  }

  @Override
  public Map<Symbol, LexicalVariable> getLexicalVariables() {
    return lexicalVariables;
  }
}
