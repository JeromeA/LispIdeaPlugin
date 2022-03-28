package org.ax1.lisp.usages;

import com.intellij.find.findUsages.FindUsagesHandler;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.SearchScope;
import org.ax1.lisp.LispUtil;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import static org.ax1.lisp.psi.impl.LispSymbolMixin.SymbolSyntaxType.*;

public class LispFindUsagesHandler extends FindUsagesHandler {
  protected LispFindUsagesHandler(@NotNull PsiElement psiElement) {
    super(psiElement);
  }

  @Override
  public @NotNull Collection<PsiReference> findReferencesToHighlight(@NotNull PsiElement target, @NotNull SearchScope searchScope) {
    LispSymbol symbol = (LispSymbol) target;
    if (symbol.getSyntaxType() == FUNCTION_DEFINITION) {
      return LispUtil.findFunctionUsages(target.getProject(), target.getText()).stream()
          .map(LispSymbol::getReference)
          .collect(Collectors.toList());
    }
    if (symbol.getSyntaxType() == VARIABLE_DEFINITION) {
      return LispUtil.findVariableUsages(symbol).stream()
          .map(LispSymbol::getReference)
          .collect(Collectors.toList());
    }
    return List.of();
  }
}
