package org.ax1.lisp.analysis;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispFile;
import org.jetbrains.annotations.NotNull;

public class LispAnnotator implements Annotator {

  @Override
  public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
    if (element instanceof LispFile){
      LispFile lispFile = (LispFile) element;
      SyntaxAnalyzer syntaxAnalyzer = new SyntaxAnalyzer(lispFile, holder);
      syntaxAnalyzer.analyze();
    }
  }
}
