package org.ax1.lisp.stubs;

import com.intellij.extapi.psi.StubBasedPsiElementBase;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.util.NlsSafe;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.IncorrectOperationException;
import org.ax1.lisp.analysis.ProjectComputedData;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispElementFactory;
import org.ax1.lisp.psi.LispStringContent;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.ax1.lisp.usages.LispStringDesignatorReference;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class LispStringDesignatorStubBase<T extends StubElement> extends StubBasedPsiElementBase<T> implements LispStringDesignator {

  public LispStringDesignatorStubBase(@NotNull T stub, @NotNull IStubElementType<?, ?> nodeType) {
    super(stub, nodeType);
  }

  public LispStringDesignatorStubBase(@NotNull ASTNode node) {
    super(node);
  }

  public LispStringDesignatorStubBase(T stub, IElementType nodeType, ASTNode node) {
    super(stub, nodeType, node);
  }

  @Override
  public PsiElement setName(@NlsSafe @NotNull String newName) throws IncorrectOperationException {
    ASTNode node = getNode();
    node.getTreeParent().replaceChild(node, createNewNode(newName));
    return this;
  }

  public ASTNode createNewNode(@NotNull String newName) {
    if (this instanceof LispStringContent) {
      return LispElementFactory.createStringContent(getProject(), newName).getNode();
    }
    return LispElementFactory.createSymbolName(getProject(), newName).getNode();
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
