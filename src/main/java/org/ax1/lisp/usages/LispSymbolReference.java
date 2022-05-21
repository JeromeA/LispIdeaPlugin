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

  private final PsiElement definition;

  public LispSymbolReference(@NotNull LispSymbol symbol, @NotNull PsiElement definition) {
    super(symbol);
    this.definition = definition;
  }

  @Override
  public @Nullable PsiElement resolve() {
    return definition;
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
