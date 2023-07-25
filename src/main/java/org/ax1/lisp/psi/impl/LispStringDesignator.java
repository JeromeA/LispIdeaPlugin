package org.ax1.lisp.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.psi.PsiNameIdentifierOwner;
import com.intellij.psi.PsiReference;
import org.jetbrains.annotations.NotNull;

public interface LispStringDesignator extends PsiNameIdentifierOwner {
  String getValue();
  ASTNode createNewNode(@NotNull String newName);

  Type getType();

  void annotate(@NotNull AnnotationHolder holder);

  // Only for definitions.
  String getDescriptionString();

  enum Type {
    DATA,
    PACKAGE_DEFINITION,
    FUNCTION_DEFINITION,
    METHOD_DEFINITION,
    VARIABLE_DEFINITION,
    PACKAGE_USAGE,
    FUNCTION_USAGE,
    VARIABLE_USAGE
  };
}
