package org.ax1.lisp.usages;

import com.intellij.find.findUsages.FindUsagesHandler;
import com.intellij.find.findUsages.FindUsagesHandlerFactory;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class LispFindUsagesHandlerFactory extends FindUsagesHandlerFactory {

  @Override
  public boolean canFindUsages(@NotNull PsiElement element) {
    if (!(element instanceof LispSymbol)) return false;
    LispSymbol symbol = (LispSymbol) element;
    return symbol.isFunctionDefinition() || symbol.isVariableDefinition();
  }

  @Override
  public @Nullable FindUsagesHandler createFindUsagesHandler(@NotNull PsiElement element, boolean forHighlightUsages) {
    return new LispFindUsagesHandler(element);
  }
}
