package org.ax1.lisp.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.HighlightSeverity;
import org.ax1.lisp.analysis.BaseLispElement;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispFile;
import org.jetbrains.annotations.NotNull;

public abstract class BaseMixinImpl extends ASTWrapperPsiElement implements BaseLispElement {

  private String errorMessage;
  private Type type;

  public BaseMixinImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public void setErrorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
    if (errorMessage != null) setType(Type.ERROR);
  }

  @Override
  public void setType(Type type) {
    this.type = type;
  }

  @Override
  public Type getType() {
    LispFile containingFile = (LispFile) getContainingFile();
    SyntaxAnalyzer.INSTANCE.analyze(containingFile);
    if (type == null) {
      type = Type.UNKNOWN;
    }
    return type;
  }

  @Override
  public void annotate(@NotNull AnnotationHolder holder) {
    if (annotateWithError(holder)) return;
    annotateWithType(holder);
  }

  protected void annotateWithType(@NotNull AnnotationHolder holder) {
  }

  private boolean annotateWithError(@NotNull AnnotationHolder holder) {
    if (errorMessage != null) {
      holder.newAnnotation(HighlightSeverity.ERROR, errorMessage)
          .range(this)
          .create();
      return true;
    }
    return false;
  }

}
