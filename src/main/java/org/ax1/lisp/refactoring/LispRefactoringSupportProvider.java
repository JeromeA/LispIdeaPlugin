package org.ax1.lisp.refactoring;

import com.intellij.lang.refactoring.RefactoringSupportProvider;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class LispRefactoringSupportProvider extends RefactoringSupportProvider {
  @Override
  public boolean isInplaceRenameAvailable(@NotNull PsiElement element, PsiElement context) {
    if (element instanceof LispSymbol) {
      LispSymbol symbol = (LispSymbol) element;
      return symbol.isVariableDefinition();
    }
    return false;
  }

  @Override
  public boolean isMemberInplaceRenameAvailable(@NotNull PsiElement element, @Nullable PsiElement context) {
    if (element instanceof LispSymbol) {
      LispSymbol symbol = (LispSymbol) element;
      return symbol.isFunctionDefinition();
    }
    return false;
  }
}
