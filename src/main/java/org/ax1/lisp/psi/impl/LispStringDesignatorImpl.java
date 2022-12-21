package org.ax1.lisp.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import org.ax1.lisp.analysis.ProjectComputedData;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispElementFactory;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispStringContent;
import org.ax1.lisp.psi.LispSymbolName;
import org.ax1.lisp.usages.LispStringDesignatorReference;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static org.ax1.lisp.analysis.symbol.SymbolDefinition.Type.FUNCTION;
import static org.ax1.lisp.analysis.symbol.SymbolDefinition.Type.VARIABLE;

public abstract class LispStringDesignatorImpl extends ASTWrapperPsiElement implements LispStringDesignator {

  public LispStringDesignatorImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public SymbolDefinition getSymbolDefinition() {
    return ProjectComputedData.getInstance(getProject()).getProjectAnalysis().getSymbolDefinition(this);
  }

  @Override
  public String getValue() {
    // TODO: take escapes into account.
    if (this instanceof LispStringContent) {
      return getText();
    }
    return getText().toUpperCase();
  }

  @Override
  public String getName() {
    return getText();
  }

  @Override
  public PsiElement setName(@NotNull String newName) {
    ASTNode newNode;
    if (this instanceof LispStringContent) {
      newNode = LispElementFactory.createStringContent(getProject(), newName).getNode();
    } else {
      newNode = LispElementFactory.createSymbolName(getProject(), newName).getNode();
    }
    ASTNode parent = getNode().getTreeParent();
    parent.replaceChild(getNode(), newNode);
    return this;
  }

  @Override
  public PsiReference getReference() {
    PsiReference symbolReference = getSymbolReference();
    if (symbolReference != null) return symbolReference;
    return getPackageReference();
  }

  @Override
  public PsiReference getSymbolReference() {
    SymbolDefinition definition = getSymbolDefinition();
    if (definition != null && definition.isUsage(this)) {
      return new LispStringDesignatorReference(this, definition.getDefinition());
    }
    return null;
  }

  @Override
  public PsiReference getPackageReference() {
    PackageDefinition packageDefinition = getPackageDefinition();
    if (packageDefinition != null && packageDefinition.isUsage(this)) {
      return new LispStringDesignatorReference(this, packageDefinition.getDefinition());
    }
    return null;
  }

  @Override
  public PackageDefinition getPackageDefinition() {
    return ProjectComputedData.getInstance(getProject()).getProjectAnalysis().getPackageDefinition(this);
  }

  @Override
  public @Nullable PsiElement getNameIdentifier() {
    return this;
  }
}
