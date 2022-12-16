package org.ax1.lisp.usages;

import com.intellij.find.findUsages.FindUsagesHandler;
import com.intellij.find.findUsages.FindUsagesHandlerFactory;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.analysis.symbol.LispDefinition;
import org.ax1.lisp.analysis.symbol.SymbolBinding;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.psi.LispSexp;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class LispFindUsagesHandlerFactory extends FindUsagesHandlerFactory {

  @Override
  public boolean canFindUsages(@NotNull PsiElement element) {
    if (element instanceof LispSexp) {
      LispSexp sexp = (LispSexp) element;
      LispDefinition definition = sexp.getPackageDefinition();
      if (definition != null) {
        return definition.isDefinition(sexp);
      }
    }
    return false;
  }

  @Override
  public @Nullable FindUsagesHandler createFindUsagesHandler(@NotNull PsiElement element, boolean forHighlightUsages) {
    return new LispFindUsagesHandler(element);
  }
}
