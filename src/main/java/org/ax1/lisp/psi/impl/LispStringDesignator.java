package org.ax1.lisp.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiNameIdentifierOwner;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.PsiReference;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.jetbrains.annotations.NotNull;

public interface LispStringDesignator extends PsiNameIdentifierOwner {
  SymbolDefinition getSymbolDefinition();
  PackageDefinition getPackageDefinition();
  PsiReference getSymbolReference();
  PsiReference getPackageReference();
  String getValue();
  ASTNode createNewNode(@NotNull String newName);
}
