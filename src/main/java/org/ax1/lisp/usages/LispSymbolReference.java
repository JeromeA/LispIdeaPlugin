package org.ax1.lisp.usages;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.ElementManipulators;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReferenceBase;
import com.intellij.util.IncorrectOperationException;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class LispSymbolReference extends PsiReferenceBase<LispSymbol> {

  public LispSymbolReference(@NotNull LispSymbol symbol) {
    super(symbol);
  }

  @Override
  public @Nullable PsiElement resolve() {
    return myElement.getSymbolBinding().getDefinition();
  }

  @Override
  public @NotNull TextRange getRangeInElement() {
    return ElementManipulators.getValueTextRange(myElement);
  }

  @Override
  public PsiElement handleElementRename(@NotNull String newElementName) throws IncorrectOperationException {
    return myElement.setName(newElementName);
  }
}
