package org.ax1.lisp.usages;

import com.intellij.find.findUsages.FindUsagesHandler;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.SearchScope;
import org.ax1.lisp.analysis.symbol.LispDefinition;
import org.ax1.lisp.psi.LispSexp;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

public class LispFindUsagesHandler extends FindUsagesHandler {
  protected LispFindUsagesHandler(@NotNull PsiElement psiElement) {
    super(psiElement);
  }

  @Override
  public @NotNull Collection<PsiReference> findReferencesToHighlight(@NotNull PsiElement target, @NotNull SearchScope searchScope) {
    if (target instanceof LispSexp) {
      LispSexp sexp = (LispSexp) target;
      LispDefinition definition = sexp.getDefinition();
      if (definition != null && definition.isDefinition(sexp)) {
        return filterByScope(definition.getUsages(), searchScope);
      }
    }
    return List.of();
  }

  @NotNull
  private List<PsiReference> filterByScope(Collection<LispSexp> usages, @NotNull SearchScope searchScope) {
    return usages.stream()
        .filter(usage -> searchScope.contains(usage.getContainingFile().getVirtualFile()))
        .map(PsiElement::getReference)
        .collect(Collectors.toList());
  }
}
