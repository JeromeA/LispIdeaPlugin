package org.ax1.lisp.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;
import org.jetbrains.annotations.NotNull;

public abstract class LispSexpMixinImpl extends ASTWrapperPsiElement implements LispSexp {

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
  public LispSymbolName getSymbolName() {
    return getSymbol() == null ? null : getSymbol().getSymbolName();
  }
}
