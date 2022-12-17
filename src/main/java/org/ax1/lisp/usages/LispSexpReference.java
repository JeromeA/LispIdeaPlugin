package org.ax1.lisp.usages;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.ElementManipulators;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReferenceBase;
import com.intellij.util.IncorrectOperationException;
import org.ax1.lisp.psi.LispSexp;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class LispSexpReference extends PsiReferenceBase<LispSexp> {

  private final PsiElement definition;
  private final TextRange textRange;

  public LispSexpReference(@NotNull LispSexp sexp, PsiElement definition, TextRange textRange) {
    super(sexp);
    this.definition = definition;
    this.textRange = textRange;
  }

  @Override
  public @Nullable PsiElement resolve() {
    return definition;
  }

  @Override
  public @NotNull TextRange getRangeInElement() {
    return textRange;
  }

  @Override
  public PsiElement handleElementRename(@NotNull String newElementName) throws IncorrectOperationException {
    return myElement.setName(newElementName);
  }
}
