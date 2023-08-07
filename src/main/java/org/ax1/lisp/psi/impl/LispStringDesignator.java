package org.ax1.lisp.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.psi.PsiNameIdentifierOwner;
import org.ax1.lisp.analysis.BaseLispElement;
import org.ax1.lisp.analysis.symbol.LexicalVariable;
import org.ax1.lisp.analysis.symbol.Package;
import org.jetbrains.annotations.NotNull;

public interface LispStringDesignator extends PsiNameIdentifierOwner, BaseLispElement {
  String getValue();
  ASTNode createNewNode(@NotNull String newName);

  void annotate(@NotNull AnnotationHolder holder);

  // Only for definitions.
  String getDescriptionString();

  // Only for Package definitions.
  Package getPackageDefinition();

  void setPackageDefinition(Package definition);

  void addFunctionDefinition(String functionName);

  void setLexicalVariable(LexicalVariable lexicalVariable);

  LexicalVariable getLexicalVariable();
}
