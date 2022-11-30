package org.ax1.lisp;

import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.analysis.SymbolBinding;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.Nullable;

public class LispDocumentationProvider extends AbstractDocumentationProvider {

  @Override
  public @Nullable @Nls String generateDoc(PsiElement element, @Nullable PsiElement originalElement) {
    if (!(element instanceof LispSymbol)) return null;
    SymbolBinding symbolBinding = ((LispSymbol) element).getSymbolDefinition();
    if (symbolBinding == null) return null;
    return symbolBinding.description;
  }
}
