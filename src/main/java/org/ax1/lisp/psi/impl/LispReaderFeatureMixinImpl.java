package org.ax1.lisp.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.AnnotatedElement;
import org.ax1.lisp.analysis.BaseLispElement;
import org.ax1.lisp.psi.LispReaderFeature;
import org.ax1.lisp.subprocess.SubprocessFeatures;
import org.jetbrains.annotations.NotNull;

import static com.intellij.lang.annotation.HighlightSeverity.INFORMATION;
import static org.ax1.lisp.parsing.LispSyntaxHighlighter.COMMENT;
import static org.ax1.lisp.parsing.LispSyntaxHighlighter.READER_MACRO;

public abstract class LispReaderFeatureMixinImpl extends ASTWrapperPsiElement
    implements PsiElement, AnnotatedElement, LispReaderFeature {

  public LispReaderFeatureMixinImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public void annotate(@NotNull AnnotationHolder holder) {
    holder.newSilentAnnotation(INFORMATION)
        .range(this)
        .textAttributes(isFeatureActive() ? READER_MACRO : COMMENT)
        .create();
  }

  private boolean isFeatureActive() {
    return SubprocessFeatures.getInstance(getProject()).eval(this);
  }

}
