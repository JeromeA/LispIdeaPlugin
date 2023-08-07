package org.ax1.lisp.analysis;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.AnnotatedElement;
import org.jetbrains.annotations.NotNull;

public class LispAnnotatorManager implements Annotator {

  @Override
  public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
    if (element instanceof AnnotatedElement) {
      ((AnnotatedElement) element).annotate(holder);
    }
  }
}
