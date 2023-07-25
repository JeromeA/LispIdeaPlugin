package org.ax1.lisp;

import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.analysis.ProjectData;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispSymbolName;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.Nullable;

import static org.ax1.lisp.psi.impl.LispStringDesignator.Type.FUNCTION_USAGE;
import static org.ax1.lisp.psi.impl.LispStringDesignator.Type.VARIABLE_USAGE;

public class LispDocumentationProvider extends AbstractDocumentationProvider {

  @Override
  public @Nullable @Nls String generateDoc(PsiElement element, @Nullable PsiElement originalElement) {
    if (element instanceof LispSymbolName) {
      return getDescription((LispSymbolName) element);
    }
    return null;
  }

  private String getDescription(LispSymbolName symbolName) {
    ProjectData projectData = ProjectData.getInstance(symbolName.getProject());
    if (symbolName.getType() == FUNCTION_USAGE) {
      return projectData.getFunctionDefinition(symbolName.getValue()).getDescriptionString();
    }
    if (symbolName.getType() == VARIABLE_USAGE) {
      return projectData.getVariableDefinition(symbolName.getValue()).getDescriptionString();
    }
    return null;
  }
}
