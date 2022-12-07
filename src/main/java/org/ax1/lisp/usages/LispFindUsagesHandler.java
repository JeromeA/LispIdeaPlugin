package org.ax1.lisp.usages;

import com.intellij.find.findUsages.FindUsagesHandler;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.SearchScope;
import org.ax1.lisp.analysis.SymbolBinding;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class LispFindUsagesHandler extends FindUsagesHandler {
  protected LispFindUsagesHandler(@NotNull PsiElement psiElement) {
    super(psiElement);
  }

  @Override
  public @NotNull Collection<PsiReference> findReferencesToHighlight(@NotNull PsiElement target, @NotNull SearchScope searchScope) {
    if (target instanceof LispSymbol) {
      LispSymbol symbol = (LispSymbol) target;
      SymbolBinding symbolBinding = symbol.getSymbolDefinition();
      if (symbolBinding.definitions.contains(symbol)) {
        return filterByScope(symbolBinding.usages, searchScope);
      }
    }
    if (target instanceof LispSexp) {
      LispSexp sexp = (LispSexp) target;
      PackageDefinition packageDefinition = sexp.getPackageDefinition();
      if (packageDefinition.getDefinition() == sexp) {
        return filterByScope(packageDefinition.usages, searchScope);
      }
    }
    return List.of();
  }

  @NotNull
  private List<PsiReference> filterByScope(Collection<? extends PsiElement> usages, @NotNull SearchScope searchScope) {
    return usages.stream()
        .filter(usage -> searchScope.contains(usage.getContainingFile().getVirtualFile()))
        .map(PsiElement::getReference)
        .collect(Collectors.toList());
  }
}
