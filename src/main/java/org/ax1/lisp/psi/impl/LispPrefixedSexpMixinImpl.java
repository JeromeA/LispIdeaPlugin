package org.ax1.lisp.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.lang.annotation.AnnotationHolder;
import org.ax1.lisp.AnnotatedElement;
import org.ax1.lisp.psi.LispPrefixedSexp;
import org.ax1.lisp.subprocess.SubprocessFeatures;
import org.jetbrains.annotations.NotNull;

import static com.intellij.lang.annotation.HighlightSeverity.INFORMATION;
import static org.ax1.lisp.parsing.LispSyntaxHighlighter.COMMENT;

public abstract class LispPrefixedSexpMixinImpl extends ASTWrapperPsiElement
    implements AnnotatedElement, LispPrefixedSexp {

  public LispPrefixedSexpMixinImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public void annotate(@NotNull AnnotationHolder holder) {
    if (!isFeatureActive()) {
      holder.newSilentAnnotation(INFORMATION)
          .range(this)
          .textAttributes(COMMENT)
          .create();
    }
  }

  private boolean isFeatureActive() {
    return SubprocessFeatures.getInstance(getProject()).isValidSexp(this);
  }
}
