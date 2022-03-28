package org.ax1.lisp.usages;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.ElementManipulators;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReferenceBase;
import com.intellij.util.IncorrectOperationException;
import org.ax1.lisp.LispUtil;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class LispFunctionReference extends PsiReferenceBase<LispSymbol> {

  private final String name;

  public LispFunctionReference(@NotNull LispSymbol symbol) {
    super(symbol);
    name = symbol.getText();
  }

  @Override
  public @Nullable PsiElement resolve() {
    Project project = myElement.getProject();
    final List<LispSymbol> functionDefinitions = LispUtil.findFunctionDefinitions(project, name);
    return functionDefinitions.size() == 1 ? functionDefinitions.get(0) : null;
  }

  @Override
  public @NotNull TextRange getRangeInElement() {
    return ElementManipulators.getValueTextRange(myElement);
  }

  @Override
  public PsiElement handleElementRename(@NotNull String newElementName) throws IncorrectOperationException {
    return myElement.setName(newElementName);
  }
}
