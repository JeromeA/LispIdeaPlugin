package org.ax1.lisp.analysis;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.psi.LispFile;
import org.ax1.lisp.psi.LispList;

import static com.intellij.lang.annotation.HighlightSeverity.INFORMATION;
import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.CONSTANT;
import static com.intellij.openapi.editor.colors.CodeInsightColors.NOT_USED_ELEMENT_ATTRIBUTES;
import static com.intellij.openapi.editor.colors.CodeInsightColors.WRONG_REFERENCES_ATTRIBUTES;
import static org.ax1.lisp.parsing.LispSyntaxHighlighter.KEYWORD;

public class Annotate {

  private final LispFile lispFile;
  private final AnnotationHolder holder;

  public Annotate(LispFile lispFile, AnnotationHolder holder) {
    this.lispFile = lispFile;
    this.holder = holder;
  }

  void highlightKeyword(LispList form) {
    highlightKeyword(form.getSexpList().get(0));
  }

  void highlightKeyword(PsiElement psiElement) {
    highlight(psiElement, KEYWORD);
  }

  void highlightConstant(PsiElement psiElement) {
    highlight(psiElement, CONSTANT);
  }

  void highlightUnknown(PsiElement psiElement, String message) {
    annotate(psiElement, HighlightSeverity.ERROR, message, WRONG_REFERENCES_ATTRIBUTES);
  }

  void highlightUnused(PsiElement psiElement, String message) {
    annotate(psiElement, HighlightSeverity.WARNING, message, NOT_USED_ELEMENT_ATTRIBUTES);
  }

  void highlight(PsiElement psiElement, TextAttributesKey constant) {
    silentlyAnnotate(psiElement, INFORMATION, constant);
  }

  void highlightError(PsiElement psiElement, String message) {
    if (holder == null || psiElement.getContainingFile() != lispFile) return;
    holder.newAnnotation(HighlightSeverity.ERROR, message)
        .range(psiElement)
        .create();
  }
  private void silentlyAnnotate(PsiElement psiElement, HighlightSeverity severity, TextAttributesKey attributes) {
    if (holder == null || psiElement.getContainingFile() != lispFile) return;
    holder.newSilentAnnotation(severity)
        .range(psiElement)
        .textAttributes(attributes)
        .create();
  }

  private void annotate(PsiElement psiElement, HighlightSeverity severity, String message, TextAttributesKey attributes) {
    if (holder == null || psiElement.getContainingFile() != lispFile) return;
    holder.newAnnotation(severity, message)
        .textAttributes(attributes)
        .range(psiElement)
        .create();
  }
}
