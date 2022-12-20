package org.ax1.lisp.refactoring;

import com.intellij.lang.refactoring.RefactoringSupportProvider;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispSymbolName;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class LispRefactoringSupportProvider extends RefactoringSupportProvider {

  @Override
  public boolean isMemberInplaceRenameAvailable(@NotNull PsiElement element, @Nullable PsiElement context) {
    return isDefinition(element);
  }

  private boolean isDefinition(@NotNull PsiElement element) {
    if (element instanceof LispSymbolName) {
      LispSymbolName symbolName = (LispSymbolName) element;
      SymbolDefinition symbolDefinition = symbolName.getSymbolDefinition();
      return symbolDefinition != null && symbolDefinition.isDefinition(symbolName);
    }
    return false;
  }
}
