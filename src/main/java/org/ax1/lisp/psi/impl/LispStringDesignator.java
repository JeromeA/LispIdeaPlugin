package org.ax1.lisp.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.psi.PsiNameIdentifierOwner;
import org.ax1.lisp.analysis.BaseLispElement;
import org.ax1.lisp.analysis.symbol.LexicalSymbol;
import org.ax1.lisp.analysis.symbol.Package;
import org.jetbrains.annotations.NotNull;

public interface LispStringDesignator extends PsiNameIdentifierOwner, BaseLispElement {
  String getLispName();
  String getPackageName();
  String getPackageContext();
  ASTNode createNewNode(@NotNull String newName);

  void annotate(@NotNull AnnotationHolder holder);

  // Only for definitions.
  String getDescriptionString();

  // Only for Package definitions.
  Package getPackageDefinition();

  void setPackageDefinition(Package definition);

  void addFunctionDefinition(String functionName, String packageContext);

  void setLexicalVariable(LexicalSymbol lexicalVariable);
  void setLexicalFunction(LexicalSymbol lexicalVariable);

  LexicalSymbol getLexicalVariable();
  LexicalSymbol getLexicalFunction();

  void setType(Type type, String packageContext);
}
