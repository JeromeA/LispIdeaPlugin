package org.ax1.lisp.usages;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.ElementManipulators;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReferenceBase;
import com.intellij.util.IncorrectOperationException;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.ax1.lisp.psi.impl.LispSexpMixin;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class LispSexpReference extends PsiReferenceBase<LispSexp> {

  private final PsiElement definition;

  public LispSexpReference(@NotNull LispSexp sexp, @NotNull PsiElement definition) {
    super(sexp);
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
