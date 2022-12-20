package org.ax1.lisp.usages;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.ElementManipulators;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReferenceBase;
import com.intellij.util.IncorrectOperationException;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class LispStringDesignatorReference extends PsiReferenceBase<LispStringDesignator> {

  private final PsiElement definition;

  public LispStringDesignatorReference(@NotNull LispStringDesignator stringDesignator, PsiElement definition) {
    super(stringDesignator);
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
