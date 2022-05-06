package org.ax1.lisp.refactoring;

import com.intellij.lang.refactoring.RefactoringSupportProvider;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class LispRefactoringSupportProvider extends RefactoringSupportProvider {

  @Override
  public boolean isInplaceRenameAvailable(@NotNull PsiElement element, PsiElement context) {
    return isLispDefinition(element);
  }

  @Override
  public boolean isMemberInplaceRenameAvailable(@NotNull PsiElement element, @Nullable PsiElement context) {
    return isLispDefinition(element);
  }

  private boolean isLispDefinition(@NotNull PsiElement element) {
    if (element instanceof LispSymbol) {
      LispSymbol symbol = (LispSymbol) element;
      return symbol.isFunctionDefinition() || symbol.isVariableDefinition();
    }
    return false;
  }
}
