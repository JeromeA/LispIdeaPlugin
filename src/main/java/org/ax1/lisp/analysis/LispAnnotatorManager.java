package org.ax1.lisp.analysis;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.AnnotatedElement;
import org.ax1.lisp.analysis.symbol.Lambda;
import org.ax1.lisp.analysis.symbol.PackageManager;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispFile;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.jetbrains.annotations.NotNull;

import static org.ax1.lisp.analysis.symbol.SymbolDefinition.Type.FUNCTION;

public class LispAnnotatorManager implements Annotator {

  @Override
  public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
    System.err.println(element.getClass().getName());
    if (element instanceof AnnotatedElement) {
      ((AnnotatedElement) element).annotate(holder);
    }
  }
}
