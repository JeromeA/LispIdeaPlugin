package org.ax1.lisp;

import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispSymbolName;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.Nullable;

public class LispDocumentationProvider extends AbstractDocumentationProvider {

  @Override
  public @Nullable @Nls String generateDoc(PsiElement element, @Nullable PsiElement originalElement) {
    if (element instanceof LispSymbolName) {
      SymbolDefinition symbolDefinition = ((LispSymbolName) element).getSymbolDefinition();
      if (symbolDefinition != null) return symbolDefinition.description;
    }
    return null;
  }
}
