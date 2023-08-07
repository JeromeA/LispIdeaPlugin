package org.ax1.lisp.stubs;

import com.intellij.extapi.psi.StubBasedPsiElementBase;
import com.intellij.lang.ASTNode;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.util.NlsSafe;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.IncorrectOperationException;
import org.ax1.lisp.analysis.ProjectData;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LexicalVariable;
import org.ax1.lisp.analysis.symbol.Package;
import org.ax1.lisp.psi.LispElementFactory;
import org.ax1.lisp.psi.LispFile;
import org.ax1.lisp.psi.LispStringContent;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.ax1.lisp.usages.LispStringDesignatorReference;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.HashSet;
import java.util.Set;

import static com.intellij.lang.annotation.HighlightSeverity.INFORMATION;
import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.*;

public class LispStringDesignatorStubBase<T extends StubElement> extends StubBasedPsiElementBase<T> implements LispStringDesignator {

  private Type type;
  private String errorMessage;
  private String descriptionString;
  private Package packageDefinition;
  private final Set<String> functionDefinitions = new HashSet<>();
  private LexicalVariable lexicalVariable;

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
    LispFile containingFile = (LispFile) getContainingFile();
    synchronized (containingFile) {
      if (type == null) {
        SyntaxAnalyzer.INSTANCE.analyze(containingFile);
      }
      if (type == null) {
        type = Type.UNKNOWN;
//        SyntaxAnalyzer.INSTANCE.analyze((LispFile)getContainingFile());
      }
    }
    return type;
  }

  @Override
  public void setType(Type type) {
    this.type = type;
  }

  @Override
  public void setErrorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  @Override
  public void annotate(@NotNull AnnotationHolder holder) {
    if (errorMessage != null) {
      holder.newAnnotation(HighlightSeverity.ERROR, errorMessage)
          .range(this)
          .create();
      return;
    }
    Type type = getType();
    if (type == Type.DATA) {
      holder.newSilentAnnotation(INFORMATION).range(this).textAttributes(CONSTANT).create();
    } else if (type == Type.KEYWORD) {
      holder.newSilentAnnotation(INFORMATION).range(this).textAttributes(KEYWORD).create();
    } else if (type == Type.UNKNOWN) {
      holder.newSilentAnnotation(INFORMATION).range(this).textAttributes(REASSIGNED_LOCAL_VARIABLE).create();
    }
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
  public Package getPackageDefinition() {
    return packageDefinition;
  }

  @Override
  public void setPackageDefinition(Package packageDefinition) {
    this.packageDefinition = packageDefinition;
  }

  @Override
  public void addFunctionDefinition(String functionName) {
    if (type == null) setType(Type.FUNCTION_DEFINITION);
    functionDefinitions.add(functionName);
  }

  @Override
  public void setLexicalVariable(LexicalVariable lexicalVariable) {
    this.lexicalVariable = lexicalVariable;
  }

  @Override
  public LexicalVariable getLexicalVariable() {
    return lexicalVariable;
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
    if (getType() == Type.FUNCTION_USAGE) {
      return getReference(projectData.getFunctionDefinition(getValue()));
    }
    if (getType() == Type.VARIABLE_USAGE) {
      return getReference(projectData.getVariableDefinition(getValue()));
    }
    if (getType() == Type.PACKAGE_USAGE) {
      return getReference(projectData.getPackageDefinition(getValue()));
    }
    if (getType() == Type.LEXICAL_VARIABLE_USAGE) {
      return getReference(lexicalVariable.definition);
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
