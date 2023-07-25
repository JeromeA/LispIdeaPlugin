package org.ax1.lisp.stubs;

import com.intellij.extapi.psi.StubBasedPsiElementBase;
import com.intellij.lang.ASTNode;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.openapi.util.NlsSafe;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.IncorrectOperationException;
import org.ax1.lisp.analysis.Highlighter;
import org.ax1.lisp.analysis.ProjectData;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispElementFactory;
import org.ax1.lisp.psi.LispFile;
import org.ax1.lisp.psi.LispStringContent;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.ax1.lisp.usages.LispStringDesignatorReference;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class LispStringDesignatorStubBase<T extends StubElement> extends StubBasedPsiElementBase<T> implements LispStringDesignator {

  private Type type;
  private String descriptionString;

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
      // Inheritance is generated code, so it's inconvenient to overload this method there.
      return LispElementFactory.createStringContent(getProject(), newName).getNode();
    }
    return LispElementFactory.createSymbolName(getProject(), newName).getNode();
  }

  @Override
  public Type getType() {
    if (type == null) {
      SyntaxAnalyzer.INSTANCE.analyze((LispFile)getContainingFile());
    }
    return type;
  }

  @Override
  public void annotate(@NotNull AnnotationHolder holder) {
    // TODO: "Function '%s' does not exist"
    // TODO: "Variable '%s' is not defined"
    // TODO: unused lexical variables
    // TODO: "Variable '%s' is never used"
    // TODO: "Function '%s' is never called"
    // TODO: argument checker
  }

  @Override
  public String getDescriptionString() {
    return descriptionString;
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
    ProjectData projectData = ProjectData.getInstance(getProject());
    if (type == Type.FUNCTION_USAGE) {
      return getReference(projectData.getFunctionDefinition(getValue()));
    }
    if (type == Type.VARIABLE_USAGE) {
      return getReference(projectData.getVariableDefinition(getValue()));
    }
    if (type == Type.PACKAGE_USAGE) {
      return getReference(projectData.getPackageDefinition(getValue()));
    }
    return null;
  }

  @Nullable
  private LispStringDesignatorReference getReference(LispStringDesignator functionDefinition) {
    if (functionDefinition != null) {
      return new LispStringDesignatorReference(this, functionDefinition);
    } else {
      return null;
    }
  }

  @Override
  public @Nullable PsiElement getNameIdentifier() {
    return this;
  }
}
