package org.ax1.lisp;

import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispSexp;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.Nullable;

public class LispDocumentationProvider extends AbstractDocumentationProvider {

  @Override
  public @Nullable @Nls String generateDoc(PsiElement element, @Nullable PsiElement originalElement) {
    if (!(element instanceof LispSexp)) return null;
    SymbolDefinition symbolDefinition = ((LispSexp) element).getSymbolDefinition();
    if (symbolDefinition == null) return null;
    return symbolDefinition.description;
  }
}
