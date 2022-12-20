package org.ax1.lisp.usages;

import com.intellij.find.findUsages.FindUsagesHandler;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.SearchScope;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;

import static com.google.common.collect.ImmutableList.toImmutableList;

public class LispFindUsagesHandler extends FindUsagesHandler {
  protected LispFindUsagesHandler(@NotNull PsiElement psiElement) {
    super(psiElement);
  }

  @Override
  public @NotNull Collection<PsiReference> findReferencesToHighlight(@NotNull PsiElement target, @NotNull SearchScope searchScope) {
    if (target instanceof LispStringDesignator) {
      LispStringDesignator stringDesignator = (LispStringDesignator) target;
      SymbolDefinition symbolDefinition = stringDesignator.getSymbolDefinition();
      if (symbolDefinition != null && symbolDefinition.isDefinition(stringDesignator)) {
        return symbolDefinition.getUsages().stream()
            .filter(usage -> searchScope.contains(usage.getContainingFile().getVirtualFile()))
            .map(LispStringDesignator::getSymbolReference)
            .collect(toImmutableList());
      }
      PackageDefinition packageDefinition = stringDesignator.getPackageDefinition();
      if (packageDefinition != null && packageDefinition.isDefinition(stringDesignator)) {
        return packageDefinition.getUsages().stream()
            .filter(usage -> searchScope.contains(usage.getContainingFile().getVirtualFile()))
            .map(LispStringDesignator::getPackageReference)
            .collect(toImmutableList());
      }
    }
    return List.of();
  }

}
