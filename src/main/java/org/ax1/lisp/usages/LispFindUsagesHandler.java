package org.ax1.lisp.usages;

import com.intellij.find.findUsages.FindUsagesHandler;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.SearchScope;
import org.ax1.lisp.analysis.symbol.LispDefinition;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.impl.LispSexpMixin;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.google.common.collect.ImmutableList.toImmutableList;

public class LispFindUsagesHandler extends FindUsagesHandler {
  protected LispFindUsagesHandler(@NotNull PsiElement psiElement) {
    super(psiElement);
  }

  @Override
  public @NotNull Collection<PsiReference> findReferencesToHighlight(@NotNull PsiElement target, @NotNull SearchScope searchScope) {
    if (target instanceof LispSexp) {
      LispSexp targetSexp = (LispSexp) target;
      SymbolDefinition symbolDefinition = targetSexp.getSymbolDefinition();
      if (symbolDefinition != null && symbolDefinition.isDefinition(targetSexp)) {
        return filterByScope(symbolDefinition.getUsages(), searchScope).stream()
            .map(LispSexpMixin::getSymbolReference)
            .collect(toImmutableList());
      }
      PackageDefinition packageDefinition = targetSexp.getPackageDefinition();
      if (packageDefinition != null && packageDefinition.isDefinition(targetSexp)) {
        return filterByScope(packageDefinition.getUsages(), searchScope).stream()
            .map(LispSexpMixin::getPackageReference)
            .collect(toImmutableList());
      }
    }
    return List.of();
  }

  @NotNull
  private List<LispSexp> filterByScope(Collection<LispSexp> usages, @NotNull SearchScope searchScope) {
    return usages.stream()
        .filter(usage -> searchScope.contains(usage.getContainingFile().getVirtualFile()))
        .collect(Collectors.toList());
  }
}
