package org.ax1.lisp.usages;

import com.intellij.find.findUsages.FindUsagesHandler;
import com.intellij.find.findUsages.FindUsagesHandlerFactory;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.analysis.SymbolBinding;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class LispFindUsagesHandlerFactory extends FindUsagesHandlerFactory {

  @Override
  public boolean canFindUsages(@NotNull PsiElement element) {
    if (element instanceof LispSymbol) {
      LispSymbol symbol = (LispSymbol) element;
      SymbolBinding symbolDefinition = symbol.getSymbolDefinition();
      return symbolDefinition != null && symbolDefinition.definitions.contains(symbol);
    }
    if (element instanceof LispSexp) {
      LispSexp sexp = (LispSexp) element;
      PackageDefinition packageDefinition = sexp.getPackageDefinition();
      return packageDefinition != null && packageDefinition.getDefinition() == sexp;
    }
    return false;
  }

  @Override
  public @Nullable FindUsagesHandler createFindUsagesHandler(@NotNull PsiElement element, boolean forHighlightUsages) {
    return new LispFindUsagesHandler(element);
  }
}
