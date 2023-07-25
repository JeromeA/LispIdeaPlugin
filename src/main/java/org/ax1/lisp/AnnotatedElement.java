package org.ax1.lisp;

import com.intellij.lang.annotation.AnnotationHolder;
import org.jetbrains.annotations.NotNull;

public interface AnnotatedElement {
  void annotate(@NotNull AnnotationHolder holder);
}
