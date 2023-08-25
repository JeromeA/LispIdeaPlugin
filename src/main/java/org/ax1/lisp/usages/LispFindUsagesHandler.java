package org.ax1.lisp.usages;

import com.google.common.collect.ImmutableList;
import com.intellij.find.findUsages.FindUsagesHandler;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.SearchScope;
import org.ax1.lisp.analysis.BaseLispElement;
import org.ax1.lisp.analysis.ProjectData;
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
      ProjectData projectData = ProjectData.getInstance(getProject());
      if (stringDesignator.getType() == LispStringDesignator.Type.LEXICAL_VARIABLE_DEFINITION) {
        return getUsageReferences(searchScope, stringDesignator.getLexicalVariable().usages);
      }
      if (stringDesignator.getType() == LispStringDesignator.Type.LEXICAL_FUNCTION_DEFINITION) {
        return getUsageReferences(searchScope, stringDesignator.getLexicalFunction().usages);
      }
      if (stringDesignator.getType() == LispStringDesignator.Type.VARIABLE_DEFINITION) {
        return getUsageReferences(searchScope, projectData.getVariableUsages(stringDesignator.getLispName()));
      }
      if (stringDesignator.getType() == LispStringDesignator.Type.FUNCTION_DEFINITION) {
        return getUsageReferences(searchScope, projectData.getFunctionUsages(stringDesignator.getLispName()));
      }
      if (stringDesignator.getType() == LispStringDesignator.Type.PACKAGE_DEFINITION) {
        return getUsageReferences(searchScope, projectData.getPackageUsages(stringDesignator.getLispName()));
      }
    }
    return List.of();
  }

  private static ImmutableList<PsiReference> getUsageReferences(@NotNull SearchScope searchScope, Collection<? extends LispStringDesignator> usages) {
    return usages.stream()
        .filter(usage -> searchScope.contains(usage.getContainingFile().getVirtualFile()))
        .map(LispStringDesignator::getReference)
        .collect(toImmutableList());
  }

}
