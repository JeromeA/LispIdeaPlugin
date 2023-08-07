package org.ax1.lisp.refactoring;

import com.intellij.lang.refactoring.RefactoringSupportProvider;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.analysis.BaseLispElement;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class LispRefactoringSupportProvider extends RefactoringSupportProvider {

  @Override
  public boolean isMemberInplaceRenameAvailable(@NotNull PsiElement element, @Nullable PsiElement context) {
    return isDefinition(element);
  }

  private boolean isDefinition(@NotNull PsiElement element) {
    if (!(element instanceof LispStringDesignator)) return false;
    LispStringDesignator stringDesignator = (LispStringDesignator) element;
    LispStringDesignator.Type type = stringDesignator.getType();
    return type == LispStringDesignator.Type.FUNCTION_DEFINITION
        || type == LispStringDesignator.Type.METHOD_DEFINITION
        || type == LispStringDesignator.Type.VARIABLE_DEFINITION
        || type == LispStringDesignator.Type.LEXICAL_VARIABLE_DEFINITION;
  }
}
