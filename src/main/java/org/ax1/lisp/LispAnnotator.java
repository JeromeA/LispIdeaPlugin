package org.ax1.lisp;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.psi.LispFile;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Set;

import static org.ax1.lisp.parsing.LispSyntaxHighlighter.FUNCTION_DECLARATION;
import static org.ax1.lisp.parsing.LispSyntaxHighlighter.KEYWORD;

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
