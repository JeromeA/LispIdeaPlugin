package org.ax1.lisp.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNameIdentifierOwner;
import com.intellij.psi.PsiReference;
import org.ax1.lisp.analysis.ProjectComputedData;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.psi.LispElementFactory;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.ax1.lisp.usages.LispSexpReference;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public abstract class LispSexpMixinImpl extends ASTWrapperPsiElement implements PsiNameIdentifierOwner, LispSexp {

  public LispSexpMixinImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public PsiReference getReference() {
    PackageDefinition packageDefinition = getPackageDefinition();
    if (packageDefinition != null && packageDefinition.usages.contains(this)) {
      return new LispSexpReference(this, packageDefinition.getDefinition());
    }
    return null;
  }

  @Override
  public PackageDefinition getPackageDefinition() {
    return ProjectComputedData.getInstance(getProject()).getProjectAnalysis().getPackage(this);
  }

  @Override
  public @Nullable PsiElement getNameIdentifier() {
    PackageDefinition packageDefinition = getPackageDefinition();
    if (packageDefinition != null && packageDefinition.getDefinition() == this) {
      return this;
    }
    return null;
  }

  @Override
  public PsiElement setName(@NotNull String newName) {
    LispSymbol newSymbol = LispElementFactory.createSymbol(getProject(), newName);
    ASTNode parent = getNode().getTreeParent();
    parent.replaceChild(getNode(), newSymbol.getNode());
    return this;
  }

}
